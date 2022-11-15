#include <assert.h>   /* for assert */
#include <stddef.h>   /* for NULL */
#include <string.h>   /* for memcpy */
#include <sys/mman.h> /* for mmap and friends */
#include <unistd.h>

#include <cstdio>
#include <cstdlib>
#include <vector>

#include "asm_x64.c"

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
      std::fprintf(stderr, "unsupported expr type\n");
      std::abort();
    }
  }
}

Mem var_at(int offset) {
  return base_disp(RDI, offset * sizeof(State{}.vars[0]));
}

enum class Destination {
  kStack,
  kAccumulator,
  kNowhere,
};

struct Imm {
  explicit Imm(int value) : value(value) {}
  int value;
};

void plug(Destination dest, Imm imm) {
  Reg tmp = RBX;
  switch (dest) {
    case Destination::kStack: {
      push_imm(imm.value);
      break;
    }
    case Destination::kAccumulator: {
      mov_reg_imm(RAX, imm.value);
      break;
    }
    case Destination::kNowhere: {
      // Nothing to do
      break;
    }
  }
}

void plug(Destination dest, Reg reg) {
  assert(reg == RAX);
  switch (dest) {
    case Destination::kStack: {
      push_reg(reg);
      break;
    }
    case Destination::kAccumulator:
    case Destination::kNowhere: {
      // Nothing to do
      break;
    }
  }
}

void plug(Destination dest, Mem mem) {
  Reg tmp = RBX;
  switch (dest) {
    case Destination::kStack: {
      mov_reg_mem(tmp, mem);
      push_reg(tmp);
      break;
    }
    case Destination::kAccumulator: {
      mov_reg_mem(RAX, mem);
      break;
    }
    case Destination::kNowhere: {
      // Nothing to do
      break;
    }
  }
}

void compile_expr(const Expr* expr, Destination dest) {
  switch (expr->type) {
    case ExprType::kIntLit: {
      int value = reinterpret_cast<const IntLit*>(expr)->value;
      plug(dest, Imm(value));
      break;
    }
    case ExprType::kAddExpr: {
      auto add = reinterpret_cast<const AddExpr*>(expr);
      compile_expr(add->left, Destination::kStack);
      compile_expr(add->right, Destination::kAccumulator);
      pop_reg(RBX);
      add_reg_reg(RAX, RBX);
      plug(dest, RAX);
      break;
    }
    case ExprType::kVarRef: {
      int offset = reinterpret_cast<const VarRef*>(expr)->offset;
      plug(dest, var_at(offset));
      break;
    }
    case ExprType::kVarAssign: {
      auto assign = reinterpret_cast<const VarAssign*>(expr);
      compile_expr(assign->right, Destination::kAccumulator);
      mov_mem_reg(var_at(assign->left->offset), RAX);
      plug(dest, RAX);
      break;
    }
    case ExprType::kLessThan: {
      auto less = reinterpret_cast<const LessThan*>(expr);
      compile_expr(less->left, Destination::kStack);
      compile_expr(less->right, Destination::kAccumulator);
      pop_reg(RBX);
      // TODO(emacs): Use cmp
      sub_reg_reg(RBX, RAX);
      uint32_t* false_ptr = jmp_if(GE, 0);
      plug(dest, Imm(1));
      uint32_t* done_ptr = jmp(0);
      patch_rel(false_ptr, here);
      plug(dest, Imm(0));
      patch_rel(done_ptr, here);
      break;
    }
    default: {
      std::fprintf(stderr, "unsupported expr type\n");
      std::abort();
    }
  }
}

void compile_stmt(const Stmt* stmt) {
  switch (stmt->type) {
    case StmtType::kExpr: {
      compile_expr(reinterpret_cast<const ExprStmt*>(stmt)->expr,
                   Destination::kNowhere);
      break;
    }
    case StmtType::kBlock: {
      auto block = reinterpret_cast<const BlockStmt*>(stmt);
      for (size_t i = 0; i < block->body.size(); i++) {
        compile_stmt(block->body[i]);
      }
      break;
    }
    case StmtType::kIf: {
      auto if_ = reinterpret_cast<const IfStmt*>(stmt);
      compile_expr(if_->cond, Destination::kAccumulator);
      and_reg_reg(RAX, RAX);  // check if falsey
      uint32_t* else_ptr = jmp_if(E, 0);
      // true:
      compile_stmt(if_->cons);
      uint32_t* exit_ptr = jmp(0);
      // false:
      patch_rel(else_ptr, here);
      compile_stmt(if_->alt);
      // exit:
      patch_rel(exit_ptr, here);
      break;
    }
    default: {
      std::fprintf(stderr, "unsupported stmt type\n");
      std::abort();
    }
  }
}

// use passed-in vars as base pointer for locals (RDI)
typedef int (*JitFunction)(int* vars);

int jit_expr(State* state, const Expr* expr) {
  const int kProgramSize = getpagesize();  // should be enough
  void* memory = ::mmap(/*addr=*/nullptr, /*length=*/kProgramSize,
                        /*prot=*/PROT_READ | PROT_WRITE,
                        /*flags=*/MAP_ANONYMOUS | MAP_PRIVATE,
                        /*filedes=*/-1, /*offset=*/0);
  assert(memory != MAP_FAILED && "mmap failed");

  here = reinterpret_cast<uint8_t*>(memory);
  // emit prologue
  push_reg(RBP);
  mov_reg_reg(RBP, RSP);
  sub_reg_imm(RSP, kNumVars);
  // emit expr
  compile_expr(expr, Destination::kAccumulator);
  // emit epilogue
  mov_reg_reg(RSP, RBP);
  pop_reg(RBP);
  ret();

  int mprotect_result = ::mprotect(memory, kProgramSize, PROT_EXEC);
  assert(mprotect_result == 0 && "mprotect failed");
  JitFunction function = *(JitFunction*)&memory;
  int result = function(state->vars);
  int munmap_result = ::munmap(memory, kProgramSize);
  assert(munmap_result == 0 && "munmap failed");
  return result;
}

void jit_stmt(State* state, const Stmt* stmt) {
  const int kProgramSize = getpagesize();  // should be enough
  void* memory = ::mmap(/*addr=*/nullptr, /*length=*/kProgramSize,
                        /*prot=*/PROT_READ | PROT_WRITE,
                        /*flags=*/MAP_ANONYMOUS | MAP_PRIVATE,
                        /*filedes=*/-1, /*offset=*/0);
  assert(memory != MAP_FAILED && "mmap failed");

  here = reinterpret_cast<uint8_t*>(memory);
  // emit prologue
  push_reg(RBP);
  mov_reg_reg(RBP, RSP);
  sub_reg_imm(RSP, kNumVars);
  // emit expr
  compile_stmt(stmt);
  // emit epilogue
  mov_reg_reg(RSP, RBP);
  pop_reg(RBP);
  ret();

  int mprotect_result = ::mprotect(memory, kProgramSize, PROT_EXEC);
  assert(mprotect_result == 0 && "mprotect failed");
  JitFunction function = *(JitFunction*)&memory;
  function(state->vars);
  int munmap_result = ::munmap(memory, kProgramSize);
  assert(munmap_result == 0 && "munmap failed");
  return;
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
      std::fprintf(stderr, "unsupported stmt type\n");
      std::abort();
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
