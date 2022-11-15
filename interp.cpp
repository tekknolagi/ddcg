#include <cstdio>
#include <cstdlib>
#include <vector>

enum class ExprType {
  kIntLit,
  kAddExpr,
  kVarRef,
  kAssign,
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

struct Assign : public Expr {
  explicit Assign(VarRef* left, Expr* right)
      : Expr(ExprType::kAssign), left(left), right(right) {}
  VarRef* left;
  Expr* right;
};

enum class StmtType {
  kExpr,
  kBlock,
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

struct State {
  int vars[26] = {};
};

int interpret(State* state, const Expr* expr) {
  switch (expr->type) {
    case ExprType::kIntLit: {
      return reinterpret_cast<const IntLit*>(expr)->value;
    }
    case ExprType::kAddExpr: {
      auto add = reinterpret_cast<const AddExpr*>(expr);
      int left = interpret(state, add->left);
      int right = interpret(state, add->right);
      return left + right;
    }
    default: {
      std::fprintf(stderr, "unsupported expr type\n");
      std::abort();
    }
  }
}

void interpret(State* state, const Stmt* stmt) {
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
  }
}

struct ExprTest {
  Expr* expr;
  int expected;
};

void test_interp(ExprTest tests[]) {
  fprintf(stderr, "Testing interpreter ");
  std::vector<size_t> failed;
  for (size_t i = 0; tests[i].expr != nullptr; i++) {
    State state;
    int result = interpret(&state, tests[i].expr);
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

int main() {
  ExprTest tests[] = {
      {new IntLit(123), 123},
      {new AddExpr(new IntLit(123), new IntLit(456)), 579},
      {nullptr, 0},
  };
  test_interp(tests);
}
