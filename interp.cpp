#include <assert.h>   /* for assert */
#include <stddef.h>   /* for NULL */
#include <string.h>   /* for memcpy */
#include <sys/mman.h> /* for mmap and friends */
#include <unistd.h>

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include "dis/assembler-x64.h"
#include "dis/dcheck.h"

using namespace dis;
using namespace dis::x64;

// TODO(max): Write parser
// TODO(max): Support else-less if

enum class ExprType {
  kIntLit,
  kAddExpr,
  kVarRef,
  kVarAssign,
  kLessThan,
};

struct Expr {
  explicit Expr(ExprType type) : type(type) {}
  ExprType type;
};

struct IntLit : public Expr {
  explicit IntLit(word value) : Expr(ExprType::kIntLit), value(value) {}
  word value;
};

struct AddExpr : public Expr {
  explicit AddExpr(Expr* left, Expr* right)
      : Expr(ExprType::kAddExpr), left(left), right(right) {}
  Expr* left;
  Expr* right;
};

struct VarRef : public Expr {
  explicit VarRef(word offset) : Expr(ExprType::kVarRef), offset(offset) {}
  word offset;
};

struct VarAssign : public Expr {
  explicit VarAssign(VarRef* left, Expr* right)
      : Expr(ExprType::kVarAssign), left(left), right(right) {}
  VarRef* left;
  Expr* right;
};

struct LessThan : public Expr {
  explicit LessThan(Expr* left, Expr* right)
      : Expr(ExprType::kLessThan), left(left), right(right) {}
  Expr* left;
  Expr* right;
};

enum class StmtType {
  kExpr,
  kBlock,
  kIf,
};

struct Stmt {
  explicit Stmt(StmtType type) : type(type) {}
  StmtType type;
};

struct ExprStmt : public Stmt {
  explicit ExprStmt(Expr* expr) : Stmt(StmtType::kExpr), expr(expr) {}
  Expr* expr;
};

struct BlockStmt : public Stmt {
  explicit BlockStmt(const std::vector<const Stmt*>& body)
      : Stmt(StmtType::kBlock), body(body) {}
  std::vector<const Stmt*> body;
};

struct IfStmt : public Stmt {
  explicit IfStmt(Expr* cond, Stmt* cons, Stmt* alt)
      : Stmt(StmtType::kIf), cond(cond), cons(cons), alt(alt) {}
  Expr* cond;
  Stmt* cons;
  Stmt* alt;
};

constexpr int kNumVars = 26;

struct State {
  State set(word offset, word value) const {
    State result = *this;
    result.vars[offset] = value;
    return result;
  }
  bool operator==(const State& other) {
    for (size_t i = 0; i < kNumVars; i++) {
      if (vars[i] != other.vars[i]) {
        return false;
      }
    }
    return true;
  }
  word vars[kNumVars] = {};
};

class Evaluator {
 public:
  virtual word interpret(State* state, const Expr* expr) = 0;
  virtual void interpret(State* state, const Stmt* stmt) = 0;
};

class Interpreter : public Evaluator {
 public:
  virtual word interpret(State* state, const Expr* expr) {
    switch (expr->type) {
      case ExprType::kIntLit: {
        return reinterpret_cast<const IntLit*>(expr)->value;
      }
      case ExprType::kAddExpr: {
        auto add = reinterpret_cast<const AddExpr*>(expr);
        word left = interpret(state, add->left);
        word right = interpret(state, add->right);
        return left + right;
      }
      case ExprType::kVarRef: {
        return state->vars[reinterpret_cast<const VarRef*>(expr)->offset];
      }
      case ExprType::kVarAssign: {
        auto assign = reinterpret_cast<const VarAssign*>(expr);
        word result = interpret(state, assign->right);
        state->vars[assign->left->offset] = result;
        return result;
      }
      case ExprType::kLessThan: {
        auto less = reinterpret_cast<const LessThan*>(expr);
        word left = interpret(state, less->left);
        word right = interpret(state, less->right);
        return left < right;
      }
      default: {
        UNREACHABLE("unsupported expr type");
        break;
      }
    }
  }
  virtual void interpret(State* state, const Stmt* stmt) {
    switch (stmt->type) {
      case StmtType::kExpr: {
        interpret(state, reinterpret_cast<const ExprStmt*>(stmt)->expr);
        break;
      }
      case StmtType::kBlock: {
        auto block = reinterpret_cast<const BlockStmt*>(stmt);
        for (size_t i = 0; i < block->body.size(); i++) {
          interpret(state, block->body[i]);
        }
        break;
      }
      case StmtType::kIf: {
        auto if_ = reinterpret_cast<const IfStmt*>(stmt);
        word result = interpret(state, if_->cond);
        if (result) {
          interpret(state, if_->cons);
        } else {
          interpret(state, if_->alt);
        }
        break;
      }
      default: {
        UNREACHABLE("unsupported stmt type");
        break;
      }
    }
  }
};

#define __ as.

enum class Destination {
  kStack,
  kAccumulator,
  kNowhere,
};

// TODO(max): Fix camelCase vs snake_case naming conventions in this file

MemoryRegion finalizeCode(Assembler* as) {
  word code_size = Utils::roundUp(as->codeSize(), getpagesize());
  void* code = ::mmap(/*addr=*/nullptr, /*length=*/code_size,
                      /*prot=*/PROT_READ | PROT_WRITE,
                      /*flags=*/MAP_ANONYMOUS | MAP_PRIVATE,
                      /*filedes=*/-1, /*offset=*/0);
  DCHECK(code != MAP_FAILED, "mmap failed: %s", ::strerror(errno));
  MemoryRegion result(code, code_size);
  as->finalizeInstructions(result);
  int mprotect_result = ::mprotect(code, code_size, PROT_EXEC);
  DCHECK(mprotect_result == 0, "mprotect failed: %s", ::strerror(errno));
  return result;
}

void unmapCode(MemoryRegion region) {
  int munmap_result = ::munmap(region.pointer(), region.size());
  DCHECK(munmap_result == 0, "munmap(%p) failed: %s", region.pointer(),
         ::strerror(errno));
}

// use passed-in vars as base pointer for locals (RDI)
typedef word (*JitFunction)(word* vars);

JitFunction codeAsFunction(MemoryRegion region) {
  void* code = region.pointer();
  return *(JitFunction*)&code;
}

class JIT : public Evaluator {
 public:
  virtual word interpret(State* state, const Expr* expr) {
    emitPrologue();
    compileExpr(expr);
    emitEpilogue();
    MemoryRegion region = finalizeCode(&as);
    JitFunction function = codeAsFunction(region);
    word result = function(state->vars);
    unmapCode(region);
    return result;
  }

  virtual void interpret(State* state, const Stmt* stmt) {
    emitPrologue();
    compileStmt(stmt);
    emitEpilogue();
    MemoryRegion region = finalizeCode(&as);
    JitFunction function = codeAsFunction(region);
    function(state->vars);
    unmapCode(region);
  }

  virtual void emitPrologue() {
    as.pushq(RBP);
    as.movq(RBP, RSP);
    as.subq(RSP, Immediate(kNumVars));
  }

  virtual void emitEpilogue() {
    as.movq(RSP, RBP);
    as.popq(RBP);
    as.ret();
  }

  word codeSize() const { return as.codeSize(); }

  Address varAt(word index) {
    return Address(RDI, index * sizeof(State{}.vars[0]));
  }

  virtual void compileExpr(const Expr* expr) = 0;

  virtual void compileStmt(const Stmt* stmt) = 0;

 protected:
  Assembler as;
};

class BaselineJIT : public JIT {
 public:
  virtual void compileExpr(const Expr* expr) {
    compileExprHelper(expr);
    __ popq(RAX);
  }

  void compileExprHelper(const Expr* expr) {
    Register tmp = RCX;
    switch (expr->type) {
      case ExprType::kIntLit: {
        word value = reinterpret_cast<const IntLit*>(expr)->value;
        __ pushq(Immediate(value));
        break;
      }
      case ExprType::kAddExpr: {
        auto add = reinterpret_cast<const AddExpr*>(expr);
        compileExprHelper(add->left);
        compileExprHelper(add->right);
        __ popq(tmp);
        __ popq(RAX);
        __ addq(RAX, tmp);
        __ pushq(RAX);
        break;
      }
      case ExprType::kVarRef: {
        int offset = reinterpret_cast<const VarRef*>(expr)->offset;
        __ movq(RAX, varAt(offset));
        __ pushq(RAX);
        break;
      }
      case ExprType::kVarAssign: {
        auto assign = reinterpret_cast<const VarAssign*>(expr);
        compileExprHelper(assign->right);
        __ movq(RAX, Address(RSP, 0));
        __ movq(varAt(assign->left->offset), RAX);
        break;
      }
      case ExprType::kLessThan: {
        auto less = reinterpret_cast<const LessThan*>(expr);
        compileExprHelper(less->left);
        compileExprHelper(less->right);
        __ popq(tmp);
        __ popq(RAX);
        __ subq(RAX, tmp);
        __ movq(RAX, Immediate(0));
        __ setcc(LESS, RAX);
        __ pushq(RAX);
        break;
      }
      default: {
        UNREACHABLE("unsupported expr type");
        break;
      }
    }
  }

  virtual void compileStmt(const Stmt* stmt) {
    switch (stmt->type) {
      case StmtType::kExpr: {
        compileExprHelper(reinterpret_cast<const ExprStmt*>(stmt)->expr);
        __ popq(RAX);
        break;
      }
      case StmtType::kBlock: {
        auto block = reinterpret_cast<const BlockStmt*>(stmt);
        for (size_t i = 0; i < block->body.size(); i++) {
          compileStmt(block->body[i]);
        }
        break;
      }
      case StmtType::kIf: {
        auto if_ = reinterpret_cast<const IfStmt*>(stmt);
        Label alt;
        Label exit;
        compileExprHelper(if_->cond);
        __ popq(RAX);
        __ cmpq(RAX, Immediate(0));
        __ jcc(EQUAL, &alt, Assembler::kNearJump);
        // true:
        compileStmt(if_->cons);
        __ jmp(&exit, Assembler::kNearJump);
        // false:
        __ bind(&alt);
        compileStmt(if_->alt);
        // exit:
        __ bind(&exit);
        break;
      }
      default: {
        UNREACHABLE("unsupported stmt type");
        break;
      }
    }
  }
};

class DestinationDrivenJIT : public JIT {
 public:
  virtual void compileExpr(const Expr* expr) {
    compileExpr(expr, Destination::kAccumulator);
  }

  void compileExpr(const Expr* expr, Destination dest) {
    Register tmp = RCX;
    switch (expr->type) {
      case ExprType::kIntLit: {
        int value = reinterpret_cast<const IntLit*>(expr)->value;
        plug(dest, Immediate(value));
        break;
      }
      case ExprType::kAddExpr: {
        auto add = reinterpret_cast<const AddExpr*>(expr);
        compileExpr(add->left, Destination::kStack);
        compileExpr(add->right, Destination::kAccumulator);
        __ popq(tmp);
        __ addq(RAX, tmp);
        plug(dest, RAX);
        break;
      }
      case ExprType::kVarRef: {
        int offset = reinterpret_cast<const VarRef*>(expr)->offset;
        plug(dest, varAt(offset));
        break;
      }
      case ExprType::kVarAssign: {
        auto assign = reinterpret_cast<const VarAssign*>(expr);
        compileExpr(assign->right, Destination::kAccumulator);
        __ movq(varAt(assign->left->offset), RAX);
        plug(dest, RAX);
        break;
      }
      case ExprType::kLessThan: {
        auto less = reinterpret_cast<const LessThan*>(expr);
        compileExpr(less->left, Destination::kStack);
        compileExpr(less->right, Destination::kAccumulator);
        Label cons;
        Label alt;
        Label exit;
        __ popq(tmp);
        __ cmpq(tmp, RAX);
        __ jcc(GREATER_EQUAL, &exit, Assembler::kNearJump);
        // true:
        plug(dest, Immediate(1));
        __ jmp(&exit, Assembler::kNearJump);
        plug(dest, Immediate(0));
        __ bind(&exit);
        break;
      }
      default: {
        UNREACHABLE("unsupported expr type");
        break;
      }
    }
  }

  virtual void compileStmt(const Stmt* stmt) {
    switch (stmt->type) {
      case StmtType::kExpr: {
        compileExpr(reinterpret_cast<const ExprStmt*>(stmt)->expr,
                    Destination::kNowhere);
        break;
      }
      case StmtType::kBlock: {
        auto block = reinterpret_cast<const BlockStmt*>(stmt);
        for (size_t i = 0; i < block->body.size(); i++) {
          compileStmt(block->body[i]);
        }
        break;
      }
      case StmtType::kIf: {
        auto if_ = reinterpret_cast<const IfStmt*>(stmt);
        compileExpr(if_->cond, Destination::kAccumulator);
        Label alt;
        Label exit;
        __ andq(RAX, RAX);  // check if falsey
        __ jcc(EQUAL, &alt, Assembler::kNearJump);
        // true:
        compileStmt(if_->cons);
        __ jmp(&exit, Assembler::kNearJump);
        // false:
        __ bind(&alt);
        compileStmt(if_->alt);
        // exit:
        __ bind(&exit);
        break;
      }
      default: {
        std::fprintf(stderr, "unsupported stmt type\n");
        std::abort();
      }
    }
  }

  void plug(Destination dest, Immediate imm) {
    Register tmp = RCX;
    switch (dest) {
      case Destination::kStack: {
        __ pushq(imm);
        break;
      }
      case Destination::kAccumulator: {
        __ movq(RAX, imm);
        break;
      }
      case Destination::kNowhere: {
        // Nothing to do
        break;
      }
    }
  }

  void plug(Destination dest, Register reg) {
    assert(reg == RAX);
    switch (dest) {
      case Destination::kStack: {
        __ pushq(reg);
        break;
      }
      case Destination::kAccumulator:
      case Destination::kNowhere: {
        // Nothing to do
        break;
      }
    }
  }

  void plug(Destination dest, Address mem) {
    Register tmp = RBX;
    switch (dest) {
      case Destination::kStack: {
        __ movq(tmp, mem);
        __ pushq(tmp);
        break;
      }
      case Destination::kAccumulator: {
        __ movq(RAX, mem);
        break;
      }
      case Destination::kNowhere: {
        // Nothing to do
        break;
      }
    }
  }
};

enum class ControlDestinationType {
  kRegister,
  kCondition,
};

struct ControlDestination {
  explicit ControlDestination(Label* next) : cons(next), alt(next) {}
  explicit ControlDestination(Label* cons, Label* alt) : cons(cons), alt(alt) {}
  static ControlDestination nowhere() {
    return ControlDestination(nullptr, nullptr);
  }
  Register reg{kNoRegister};
  Label* cons;
  Label* alt;
};

class ControlDestinationDrivenJIT : public JIT {
 public:
  virtual void compileExpr(const Expr* expr) {
    Label next;
    compileExpr(expr, Destination::kAccumulator,
                ControlDestination(&next, &next));
    __ bind(&next);
  }

  void compileExpr(const Expr* expr, Destination dest,
                   ControlDestination cdest) {
    Register tmp = RCX;
    switch (expr->type) {
      case ExprType::kIntLit: {
        word value = reinterpret_cast<const IntLit*>(expr)->value;
        plug(dest, cdest, Immediate(value));
        break;
      }
      case ExprType::kAddExpr: {
        auto add = reinterpret_cast<const AddExpr*>(expr);
        compileExpr(add->left, Destination::kStack, cdest);
        compileExpr(add->right, Destination::kAccumulator, cdest);
        __ popq(tmp);
        __ addq(RAX, tmp);
        plug(dest, cdest, RAX);
        break;
      }
      case ExprType::kVarRef: {
        word offset = reinterpret_cast<const VarRef*>(expr)->offset;
        plug(dest, cdest, varAt(offset));
        break;
      }
      case ExprType::kVarAssign: {
        auto assign = reinterpret_cast<const VarAssign*>(expr);
        compileExpr(assign->right, Destination::kAccumulator, cdest);
        __ movq(varAt(assign->left->offset), RAX);
        plug(dest, cdest, RAX);
        break;
      }
      case ExprType::kLessThan: {
        auto less = reinterpret_cast<const LessThan*>(expr);
        compileExpr(less->left, Destination::kStack, cdest);
        compileExpr(less->right, Destination::kAccumulator, cdest);
        __ popq(tmp);
        __ cmpq(tmp, RAX);
        plug(dest, cdest, LESS);
        break;
      }
      default: {
        UNREACHABLE("unsupported expr type");
        break;
      }
    }
  }

  virtual void compileStmt(const Stmt* stmt) {
    Label next;
    compileStmt(stmt, ControlDestination(&next, &next));
    __ bind(&next);
  }

  void compileStmt(const Stmt* stmt, ControlDestination cdest) {
    switch (stmt->type) {
      case StmtType::kExpr: {
        compileExpr(reinterpret_cast<const ExprStmt*>(stmt)->expr,
                    Destination::kNowhere, cdest);
        break;
      }
      case StmtType::kBlock: {
        auto block = reinterpret_cast<const BlockStmt*>(stmt);
        for (size_t i = 0; i < block->body.size(); i++) {
          Label next;
          compileStmt(block->body[i], ControlDestination(&next, &next));
          __ bind(&next);
        }
        break;
      }
      case StmtType::kIf: {
        auto if_ = reinterpret_cast<const IfStmt*>(stmt);
        Label cons;
        Label alt;
        compileExpr(if_->cond, Destination::kNowhere,
                    ControlDestination(&cons, &alt));
        // true:
        __ bind(&cons);
        compileStmt(if_->cons, cdest);
        Label exit;
        __ jmp(&exit, Assembler::kNearJump);
        // false:
        __ bind(&alt);
        compileStmt(if_->alt, cdest);
        // exit:
        __ bind(&exit);
        break;
      }
      default: {
        UNREACHABLE("unsupported stmt type");
        break;
      }
    }
  }

  void plug(Destination dest, ControlDestination cdest, Condition cond) {
    switch (dest) {
      case Destination::kStack: {
        UNREACHABLE("TODO(max): implement plug(stack, cond)");
        break;
      }
      case Destination::kAccumulator: {
        Label materialize_true;
        __ jcc(cond, &materialize_true, Assembler::kNearJump);
        __ movq(RAX, Immediate(0));
        __ jmp(cdest.alt, Assembler::kNearJump);
        __ bind(&materialize_true);
        __ movq(RAX, Immediate(1));
        __ jmp(cdest.cons, Assembler::kNearJump);
        break;
      }
      case Destination::kNowhere: {
        __ jcc(cond, cdest.cons, Assembler::kNearJump);
        __ jmp(cdest.alt, Assembler::kNearJump);
        break;
      }
    }
  }

  void plug(Destination dest, ControlDestination cdest, Immediate imm) {
    switch (dest) {
      case Destination::kStack: {
        __ pushq(imm);
        break;
      }
      case Destination::kAccumulator: {
        __ movq(RAX, imm);
        break;
      }
      case Destination::kNowhere: {
        // Nothing to do; not supposed to be materialized anywhere. Likely from
        // an ExprStmt.
        if (imm.value()) {
          __ jmp(cdest.cons, Assembler::kNearJump);
        } else {
          __ jmp(cdest.alt, Assembler::kNearJump);
        }
        break;
      }
    }
  }

  void plug(Destination dest, ControlDestination cdest, Register reg) {
    switch (dest) {
      case Destination::kStack: {
        UNREACHABLE("TODO(max): see how to generate this code");
        __ pushq(reg);
        break;
      }
      case Destination::kAccumulator: {
        // Nothing to do; reg already in RAX or it's not supposed to materialize
        // anywhere
        DCHECK(reg == RAX, "expect RAX to be accumulator");
        break;
      }
      case Destination::kNowhere: {
        __ cmpq(reg, Immediate(0));
        __ jcc(EQUAL, cdest.alt, Assembler::kNearJump);
        __ jmp(cdest.cons, Assembler::kNearJump);
        break;
      }
    }
  }

  void plug(Destination dest, ControlDestination cdest, Address mem) {
    Register tmp = RCX;
    switch (dest) {
      case Destination::kStack: {
        __ movq(tmp, mem);
        __ pushq(tmp);
        break;
      }
      case Destination::kAccumulator: {
        __ movq(RAX, mem);
        break;
      }
      case Destination::kNowhere: {
        UNREACHABLE("TODO(max): see how to generate this code");
        break;
      }
    }
  }
};

// TODO(max): Unify ExprTest and StmtTest

struct ExprTest {
  State state;
  Expr* expr;
  word expected;
};

struct StmtTest {
  Stmt* stmt;
  State expected;
};

void print_results(const std::vector<size_t>& failed) {
  if (failed.size()) {
    fprintf(stderr, "Failed tests:");
    for (size_t test : failed) {
      fprintf(stderr, " %zu", test);
    }
    fprintf(stderr, "\n");
  }
}

template <typename T>
void test_interp(ExprTest tests[]) {
  std::vector<size_t> failed;
  for (size_t i = 0; tests[i].expr != nullptr; i++) {
    word result = T{}.interpret(&tests[i].state, tests[i].expr);
    if (result == tests[i].expected) {
      fprintf(stderr, ".");
    } else {
      failed.push_back(i);
      fprintf(stderr, "E");
    }
  }
  fprintf(stderr, "\n");
  print_results(failed);
}

template <typename T>
void test_interp(StmtTest tests[]) {
  std::vector<size_t> failed;
  for (size_t i = 0; tests[i].stmt != nullptr; i++) {
    State state;
    T{}.interpret(&state, tests[i].stmt);
    if (state == tests[i].expected) {
      fprintf(stderr, ".");
    } else {
      failed.push_back(i);
      fprintf(stderr, "E");
    }
  }
  fprintf(stderr, "\n");
  print_results(failed);
}

int main() {
  ExprTest expr_tests[] = {
      {State{}, new IntLit(123), 123},
      {State{}, new AddExpr(new IntLit(123), new IntLit(456)), 579},
      {State{}.set(3, 123), new VarRef(3), 123},
      {State{}, new VarAssign(new VarRef(3), new IntLit(123)), 123},
      {State{}, new LessThan(new IntLit(1), new IntLit(2)), 1},
      {State{}, new LessThan(new IntLit(2), new IntLit(2)), 0},
      {State{}, new LessThan(new IntLit(3), new IntLit(2)), 0},
      // TODO(max): Test compound conditionals
      {State{}, nullptr, 0},
  };
  StmtTest stmt_tests[] = {
      {new ExprStmt(new IntLit(123)), State{}},
      {new ExprStmt(new VarAssign(new VarRef(3), new IntLit(123))),
       State{}.set(3, 123)},
      {new BlockStmt({
           new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
           new ExprStmt(new VarAssign(new VarRef(1), new IntLit(456))),
       }),
       State{}.set(0, 123).set(1, 456)},
      {new IfStmt(new IntLit(1),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 123)},
      {new IfStmt(new IntLit(7),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 123)},
      {new IfStmt(new IntLit(-7),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 123)},
      {new IfStmt(new IntLit(0),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 456)},
      {new IfStmt(new LessThan(new IntLit(1), new IntLit(2)),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 123)},
      {new IfStmt(new LessThan(new IntLit(2), new IntLit(2)),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 456)},
      {new IfStmt(new LessThan(new IntLit(3), new IntLit(2)),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 456)},
      {new BlockStmt({
           new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
           new ExprStmt(new VarAssign(new VarRef(1), new IntLit(456))),
           new ExprStmt(new VarAssign(
               new VarRef(2), new AddExpr(new VarRef(0), new VarRef(1)))),
       }),
       State{}.set(0, 123).set(1, 456).set(2, 123 + 456)},
      // TODO(max): Test nested if
      {nullptr, State{}},
  };
  fprintf(stderr, "Testing interpreter (expr) ");
  test_interp<Interpreter>(expr_tests);
  fprintf(stderr, "Testing interpreter (stmt) ");
  test_interp<Interpreter>(stmt_tests);
  fprintf(stderr, "Testing baseline jit (expr) ");
  test_interp<BaselineJIT>(expr_tests);
  fprintf(stderr, "Testing baseline jit (stmt) ");
  test_interp<BaselineJIT>(stmt_tests);
  fprintf(stderr, "Testing destination jit (expr) ");
  test_interp<DestinationDrivenJIT>(expr_tests);
  fprintf(stderr, "Testing destination jit (stmt) ");
  test_interp<DestinationDrivenJIT>(stmt_tests);
  fprintf(stderr, "Testing control destination jit (expr) ");
  test_interp<ControlDestinationDrivenJIT>(expr_tests);
  fprintf(stderr, "Testing control destination jit (stmt) ");
  test_interp<ControlDestinationDrivenJIT>(stmt_tests);
}
