---
title: Destination-driven code generation
---

I saw [Phil Eaton][eatonphil] post about this paper, [Destination-Driven Code
Generation][ddcg-paper] (PDF) on Twitter. He posted about how the V8 team used
it in the early days to do really fast but still half-decent codegen for their
compiler. It sounded really interesting, so I skimmed the paper, saw a lot of
greek symbols, and bailed out. Then I saw that Phil had posted [a
talk][ddcg-talk] (PDF) by a V8 engineer that laid out the paper's core ideas
really clearly. To the paper's credit, the actual text is extremely clear. I
think I just wasn't ready for Greek letters that day.

[eatonphil]: https://twitter.com/phil_eaton/
[ddcg-paper]: https://legacy.cs.indiana.edu/~dyb/pubs/ddcg.pdf
[ddcg-talk]: https://raw.githubusercontent.com/eatonphil/one-pass-code-generation-in-v8/main/One-pass%20Code%20Generation%20in%20V8.pdf

In any case, while V8 ostensibly used this strategy in their early codegen, I
couldn't find their implementation, nor could I find any other
implementation[^jsc] of this paper. So I decided to write one!

[^jsc]: I was writing this post, I discovered Phil's
    [implementation](https://github.com/eatonphil/jsc) of this paper in his
    ahead-of-time JavaScript compiler, *jsc* that compiles JS to Node C-API
    calls. Note that this is a different `jsc` than Apple's
    [JavaScriptCore](https://github.com/WebKit/WebKit/tree/main/Source/JavaScriptCore),
    the runtime that powers WebKit.

    Then I decided to look even deeper into the V8 source history for all
    commits by the author of the talk, Kevin Millikin, and eventually found
    [this commit](https://github.com/v8/v8/commit/1528bf7240586d876d2deef18d1e1b4302866c0b).

    It appears to be the cutover point between the "classic" compiler and the
    [Crankshaft][crankshaft][^crankshaft] compiler---and the classic compiler
    is the one referenced in the talk! See [codegen-x64.h][codegen-x64.h] and
    [codegen-x64.cc][codegen-x64.cc] for more info.

    [crankshaft]: https://blog.chromium.org/2010/12/new-crankshaft-for-v8.html
    [codegen-x64.h]: https://github.com/v8/v8/blob/1528bf7240586d876d2deef18d1e1b4302866c0b/src/x64/codegen-x64.h
    [codegen-x64.cc]: https://github.com/v8/v8/blob/1528bf7240586d876d2deef18d1e1b4302866c0b/src/x64/codegen-x64.cc

[^crankshaft]: See also Andy Wingo's [closer look at
  Crankshaft][wingo-crankshaft] and Jay Conrod's [tour of V8:
  Crankshaft][conrod-crankshaft].

    [wingo-crankshaft]: https://wingolog.org/archives/2011/08/02/a-closer-look-at-crankshaft-v8s-optimizing-compiler
    [conrod-crankshaft]: https://www.jayconrod.com/posts/54/a-tour-of-v8-crankshaft-the-optimizing-compiler

I followed along with the talk in C++. I decided to write a reference
interpreter first to help me nail down the expression types and their expected
behavior---and then wrote a small test suite to exercise it and find bugs. I
didn't get so far as writing a full parser or anything[^tiger-impl], just the
AST types. Here are the expression datatypes[^unique-ptr]. They look similar
enough to the talk:

[^tiger-impl]: I have half of a small [Tiger][tiger] implementation in C++
    sitting around, which includes a reasonable homemade lexer and parser. I
    could probably repurpose those for the blog post if people want a full
    end-to-end playground for this code generation style.

    [tiger]: https://www.cs.princeton.edu/~appel/modern/

[^unique-ptr]: I initially thought about using `std::unique_ptr` to manage the
    AST ownership here, but it kind of gets in the way of what is really just
    an elaborate test harness. Then I thought about bump allocating the nodes,
    since I already have a small bump allocator that I wrote for Cinder. Then I
    realized that that bump allocator didn't work for heterogeneous types (the
    Cinder allocator only ever allocated one type per allocator). Then I looked
    at `std::align` for a bit and decided I didn't care so much. So here we
    are, with a bunch of leaks. Ah well.

```c++
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
```

It's kind of weird that variable assignment is an expression, but since it
returns a value (the right-hand side of the assignment), I suppose it makes
sense. That seems to be [how C does it][c99-spec] (PDF). According to section
6.5.16 (page 91)[^thanks-gurity]:

[c99-spec]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

[^thanks-gurity]: Thanks, Gurity, for your
    [StackOverflow answer](https://stackoverflow.com/a/25578647/569183).

```
assignment-expression:
  conditional-expression
  unary-expression assignment-operator assignment-expression
```

I also wrote some statement datatypes:

```c++
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
```

The talk assumes code generation, but since I am also writing an interpreter, I
added a little data structure I call `State` that holds an array of slots for
variables. This would normally be part of the native call frame, probably an
array starting at the base pointer (RBP).

```c++
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
```

I didn't really want to go through and figure out how many variables were
needed in the AST, so I fixed the number of variables at 26. I had a vague
notion that if I were writing a little parser, I could use single-letter
variable names and cleanly map those to indices... but that never materialized.

<br />
<hr style="width: 100px;" />
<!-- Footnotes -->
