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
  explicit IntLit(int value) : Expr(ExprType::kIntLit), value(value) {}
  int value;
};

struct AddExpr : public Expr {
  explicit AddExpr(Expr* left, Expr* right)
      : Expr(ExprType::kAddExpr), left(left), right(right) {}
  Expr* left;
  Expr* right;
};

struct VarRef : public Expr {
  explicit VarRef(int offset) : Expr(ExprType::kVarRef), offset(offset) {}
  int offset;
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
  State set(int offset, int value) const {
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
  int vars[kNumVars] = {};
};

int interpret_expr(State* state, const Expr* expr) {
  switch (expr->type) {
    case ExprType::kIntLit: {
      return reinterpret_cast<const IntLit*>(expr)->value;
    }
    case ExprType::kAddExpr: {
      auto add = reinterpret_cast<const AddExpr*>(expr);
      int left = interpret_expr(state, add->left);
      int right = interpret_expr(state, add->right);
      return left + right;
    }
    case ExprType::kVarRef: {
      return state->vars[reinterpret_cast<const VarRef*>(expr)->offset];
    }
    case ExprType::kVarAssign: {
      auto assign = reinterpret_cast<const VarAssign*>(expr);
      int result = interpret_expr(state, assign->right);
      state->vars[assign->left->offset] = result;
      return result;
    }
    case ExprType::kLessThan: {
      auto less = reinterpret_cast<const LessThan*>(expr);
      int left = interpret_expr(state, less->left);
      int right = interpret_expr(state, less->right);
      return left < right;
    }
    default: {
      UNREACHABLE("unsupported expr type");
      break;
    }
  }
}

#define __ as->

Address var_at(int index) {
  return Address(RDI, index * sizeof(State{}.vars[0]));
}

enum class Destination {
  kStack,
  kAccumulator,
  kNowhere,
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

void plug(Assembler* as, Destination dest, ControlDestination cdest,
          Condition cond) {
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

void plug(Assembler* as, Destination dest, ControlDestination cdest,
          Immediate imm) {
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

void plug(Assembler* as, Destination dest, ControlDestination cdest,
          Register reg) {
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

void plug(Assembler* as, Destination dest, ControlDestination cdest,
          Address mem) {
  Register tmp = RBX;
  switch (dest) {
    case Destination::kStack: {
      UNREACHABLE("TODO(max): see how to generate this code");
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

void compile_expr(Assembler* as, const Expr* expr, Destination dest,
                  ControlDestination cdest) {
  switch (expr->type) {
    case ExprType::kIntLit: {
      int value = reinterpret_cast<const IntLit*>(expr)->value;
      plug(as, dest, cdest, Immediate(value));
      break;
    }
    case ExprType::kAddExpr: {
      auto add = reinterpret_cast<const AddExpr*>(expr);
      compile_expr(as, add->left, Destination::kStack, cdest);
      compile_expr(as, add->right, Destination::kAccumulator, cdest);
      __ popq(RBX);
      __ addq(RAX, RBX);
      plug(as, dest, cdest, RAX);
      break;
    }
    case ExprType::kVarRef: {
      int offset = reinterpret_cast<const VarRef*>(expr)->offset;
      plug(as, dest, cdest, var_at(offset));
      break;
    }
    case ExprType::kVarAssign: {
      auto assign = reinterpret_cast<const VarAssign*>(expr);
      compile_expr(as, assign->right, Destination::kAccumulator, cdest);
      __ movq(var_at(assign->left->offset), RAX);
      plug(as, dest, cdest, RAX);
      break;
    }
    case ExprType::kLessThan: {
      auto less = reinterpret_cast<const LessThan*>(expr);
      compile_expr(as, less->left, Destination::kStack, cdest);
      compile_expr(as, less->right, Destination::kAccumulator, cdest);
      __ popq(RBX);
      __ cmpq(RBX, RAX);
      plug(as, dest, cdest, LESS);
      break;
    }
    default: {
      UNREACHABLE("unsupported expr type");
      break;
    }
  }
}

void compile_stmt(Assembler* as, const Stmt* stmt, ControlDestination cdest) {
  switch (stmt->type) {
    case StmtType::kExpr: {
      compile_expr(as, reinterpret_cast<const ExprStmt*>(stmt)->expr,
                   Destination::kNowhere, cdest);
      break;
    }
    case StmtType::kBlock: {
      auto block = reinterpret_cast<const BlockStmt*>(stmt);
      for (size_t i = 0; i < block->body.size(); i++) {
        Label next;
        compile_stmt(as, block->body[i], ControlDestination(&next, &next));
        __ bind(&next);
      }
      break;
    }
    case StmtType::kIf: {
      auto if_ = reinterpret_cast<const IfStmt*>(stmt);
      Label cons;
      Label alt;
      compile_expr(as, if_->cond, Destination::kNowhere,
                   ControlDestination(&cons, &alt));
      // true:
      __ bind(&cons);
      compile_stmt(as, if_->cons, cdest);
      Label exit;
      __ jmp(&exit, Assembler::kNearJump);
      // false:
      __ bind(&alt);
      compile_stmt(as, if_->alt, cdest);
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

void emitPrologue(Assembler* as) {
  __ pushq(RBP);
  __ movq(RBP, RSP);
  __ subq(RSP, Immediate(kNumVars));
}

// TODO(max): Fix camelCase vs snake_case naming conventions in this file
void emitEpilogue(Assembler* as) {
  __ movq(RSP, RBP);
  __ popq(RBP);
  __ ret();
}

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
typedef int (*JitFunction)(int* vars);

JitFunction codeAsFunction(MemoryRegion region) {
  void* code = region.pointer();
  return *(JitFunction*)&code;
}

int jit_expr(State* state, const Expr* expr) {
  Assembler as;
  emitPrologue(&as);
  Label next;
  compile_expr(&as, expr, Destination::kAccumulator, ControlDestination(&next));
  as.bind(&next);
  emitEpilogue(&as);

  MemoryRegion region = finalizeCode(&as);
  JitFunction function = codeAsFunction(region);
  int result = function(state->vars);
  unmapCode(region);
  return result;
}

void jit_stmt(State* state, const Stmt* stmt) {
  Assembler as;
  emitPrologue(&as);
  Label next;
  compile_stmt(&as, stmt, ControlDestination(&next));
  as.bind(&next);
  emitEpilogue(&as);

  MemoryRegion region = finalizeCode(&as);
  JitFunction function = codeAsFunction(region);
  function(state->vars);
  unmapCode(region);
}

void interpret_stmt(State* state, const Stmt* stmt) {
  switch (stmt->type) {
    case StmtType::kExpr: {
      interpret_expr(state, reinterpret_cast<const ExprStmt*>(stmt)->expr);
      break;
    }
    case StmtType::kBlock: {
      auto block = reinterpret_cast<const BlockStmt*>(stmt);
      for (size_t i = 0; i < block->body.size(); i++) {
        interpret_stmt(state, block->body[i]);
      }
      break;
    }
    case StmtType::kIf: {
      auto if_ = reinterpret_cast<const IfStmt*>(stmt);
      int result = interpret_expr(state, if_->cond);
      if (result) {
        interpret_stmt(state, if_->cons);
      } else {
        interpret_stmt(state, if_->alt);
      }
      break;
    }
    default: {
      UNREACHABLE("unsupported stmt type");
      break;
    }
  }
}

struct ExprTest {
  State state;
  Expr* expr;
  int expected;
};

struct StmtTest {
  Stmt* stmt;
  State expected;
};

typedef int ExprInterpreter(State* state, const Expr* expr);
typedef void StmtInterpreter(State* state, const Stmt* stmt);

void test_interp(ExprTest tests[], ExprInterpreter interpret) {
  std::vector<size_t> failed;
  for (size_t i = 0; tests[i].expr != nullptr; i++) {
    int result = interpret(&tests[i].state, tests[i].expr);
    if (result == tests[i].expected) {
      fprintf(stderr, ".");
    } else {
      failed.push_back(i);
      fprintf(stderr, "E");
    }
  }
  fprintf(stderr, "\n");
  if (failed.size()) {
    fprintf(stderr, "Failed tests:");
    for (size_t test : failed) {
      fprintf(stderr, " %zu", test);
    }
    fprintf(stderr, "\n");
  }
}

void test_interp(StmtTest tests[], StmtInterpreter interpret) {
  std::vector<size_t> failed;
  for (size_t i = 0; tests[i].stmt != nullptr; i++) {
    State state;
    interpret(&state, tests[i].stmt);
    if (state == tests[i].expected) {
      fprintf(stderr, ".");
    } else {
      failed.push_back(i);
      fprintf(stderr, "E");
    }
  }
  fprintf(stderr, "\n");
  if (failed.size()) {
    fprintf(stderr, "Failed tests:");
    for (size_t test : failed) {
      fprintf(stderr, " %zu", test);
    }
    fprintf(stderr, "\n");
  }
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
      // TODO(max): Test nested if
      {nullptr, State{}},
  };
  fprintf(stderr, "Testing interpreter (expr) ");
  test_interp(expr_tests, interpret_expr);
  fprintf(stderr, "Testing jit (expr) ");
  test_interp(expr_tests, jit_expr);
  fprintf(stderr, "Testing interpreter (stmt) ");
  test_interp(stmt_tests, interpret_stmt);
  fprintf(stderr, "Testing jit (stmt) ");
  test_interp(stmt_tests, jit_stmt);
}
