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
implementation[^jsc] of this paper. So I decided to [write
one](https://github.com/tekknolagi/ddcg)!

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
abstract syntax tree (AST) types[^unique-ptr]. We'll start with them.

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

## Datatypes

Right. Let's look at some ASTs. There are not so many, and I try to match them
up with the types shown in the talk. There are five types of expression:

* integer literal
* integer add
* variable load
* variable store
* less-than comparison

and an abstract base class.

```c++
// Shorthand for a machine word's worth of data.
typedef intptr_t word;

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
sense. That seems to be [how C does it][c99-spec] (PDF), according to section
6.5.16 (page 91)[^thanks-gurity]:

[c99-spec]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

[^thanks-gurity]: Thanks, Gurity, for your
    [StackOverflow answer](https://stackoverflow.com/a/25578647/569183).

```
assignment-expression:
  conditional-expression
  unary-expression assignment-operator assignment-expression
```

The expressions are what you might expect from a little language. For example,
`v0 = v1 + v2` corresponds to the C++ AST

```c++
new VarAssign(new VarRef(0), new AddExpr(new VarRef(1), new VarRef(2)))
```

The statements are a little different. Unlike expressions, which return values,
statements return no values and are only there for effect and sequencing.

We have three kinds:

* expression statements (`expr;`)
* block statements (`{ a; b; }`)
* and if-statements (`if (a) { ...} else { ... }`)

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

I went a little terse on the variable naming: `cond` is short for "condition",
the if-expression in the parentheses; `cons` is short for "consequent", the
if-true case; `alt` is short for "alternate", the if-false case. I feel like I
heard these terms first in a Lisp scenario.

The talk assumes code generation, but since I am also writing an interpreter, I
added a little data structure I call `State` that holds an array of slots for
variables. This would normally be part of the native call frame, probably an
array starting at the base pointer (RBP).

```c++
constexpr word kNumVars = 26;

struct State {
  State set(word offset, word value) const {
    State result = *this;
    result.vars[offset] = value;
    return result;
  }
  // operator== is used for testing only, to check for the desired variable
  // assignment effects
  bool operator==(const State& other) {
    for (word i = 0; i < kNumVars; i++) {
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

## Interpreter

So! Now we have our datatypes. Let's think about how we want to structure our
interpreter. In abstract, we have two functions: one to evaluate expressions
and one to evaluate statements. Both need a `State` because both can have side
effects.

```c++
class Evaluator {
 public:
  virtual word interpret(State* state, const Expr* expr) = 0;
  virtual void interpret(State* state, const Stmt* stmt) = 0;
};
```

Our first evaluator is a little tree-walk interpreter. It implements both
functions. Here's the expression evaluator:

```c++
class Interpreter : public Evaluator {
 public:
  virtual word interpret(State* state, const Expr* expr) {
    switch (expr->type) {
      case ExprType::kIntLit: {
        return static_cast<const IntLit*>(expr)->value;
      }
      case ExprType::kAddExpr: {
        auto add = static_cast<const AddExpr*>(expr);
        word left = interpret(state, add->left);
        word right = interpret(state, add->right);
        return left + right;
      }
      case ExprType::kVarRef: {
        return state->vars[static_cast<const VarRef*>(expr)->offset];
      }
      case ExprType::kVarAssign: {
        auto assign = static_cast<const VarAssign*>(expr);
        word result = interpret(state, assign->right);
        state->vars[assign->left->offset] = result;
        return result;
      }
      case ExprType::kLessThan: {
        auto less = static_cast<const LessThan*>(expr);
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
  // ...
};
```

Note that `interpret(State*, Expr*)` returns a `word` because expressions have
values. `interpret(State*, Stmt*)`, on the other hand, returns no values:

```c++
class Interpreter : public Evaluator {
 public:
  // ...
  virtual void interpret(State* state, const Stmt* stmt) {
    switch (stmt->type) {
      case StmtType::kExpr: {
        interpret(state, static_cast<const ExprStmt*>(stmt)->expr);
        break;
      }
      case StmtType::kBlock: {
        auto block = static_cast<const BlockStmt*>(stmt);
        for (size_t i = 0; i < block->body.size(); i++) {
          interpret(state, block->body[i]);
        }
        break;
      }
      case StmtType::kIf: {
        auto if_ = static_cast<const IfStmt*>(stmt);
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
```

There are no compound conditionals, but if you were to add them, the
interpreter and test suite would be a good place to specify the semantics. (Is
`and` eager? Short-circuiting? etc)

## Tests

Now, a good test-driven-development (TDD) practitioner would have written tests
first and implementation second. But I am not that and I did not do that. So
here we arrive at the tests after writing the implementation.

Each expression test is comprised of a beginning `State`, an `Expr` to
evaluate, and the expected result from evaluating the expression.

```c++
struct ExprTest {
  State state;
  Expr* expr;
  word expected;
};
```

There is a beginning state so we can test the implementation of `VarRef`
independently of the implementation of `VarAssign`; otherwise, we would need to
use both at the same time.

```c++
int main() {
  ExprTest expr_tests[] = {
      {State{}, new IntLit(123), 123},
      {State{}, new AddExpr(new IntLit(123), new IntLit(456)), 579},
      {State{}.set(3, 123), new VarRef(3), 123},
      {State{}, new VarAssign(new VarRef(3), new IntLit(123)), 123},
      {State{}, new LessThan(new IntLit(1), new IntLit(2)), 1},
      {State{}, new LessThan(new IntLit(2), new IntLit(2)), 0},
      {State{}, new LessThan(new IntLit(3), new IntLit(2)), 0},
      {State{}, nullptr, 0},  // Sentinel
  };
  // ...
}
```

The statement tests are similar, but simpler: each test has a `Stmt` and an
expected end `State`. There is no beginning `State` supplied.

```c++
struct StmtTest {
  Stmt* stmt;
  State expected;
};
```

I suppose we could have re-used the same test struct, since expressions can
have side effects too. Hmm. I'll think about making this simpler later.

There are some more `Stmt` tests than `Expr` because of the interactions
between multiple statements. `IfStmt` in particular has a bunch of cases.

```c++
int main() {
  // ...
  StmtTest stmt_tests[] = {
      {new ExprStmt(new IntLit(123)), State{}},
      {new ExprStmt(new VarAssign(new VarRef(3), new IntLit(123))),
       State{}.set(3, 123)},
      {new IfStmt(new IntLit(1),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                  new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456)))),
       State{}.set(0, 123)},
      // ...
      {new BlockStmt({
           new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
           new ExprStmt(new VarAssign(new VarRef(1), new IntLit(456))),
           new ExprStmt(new VarAssign(
               new VarRef(2), new AddExpr(new VarRef(0), new VarRef(1)))),
       }),
       State{}.set(0, 123).set(1, 456).set(2, 123 + 456)},
      {nullptr, State{}},  // Sentinel
  };
  // ...
}
```

Now that we have the tests, we can run them on our implementations. I made the
test runner polymorphic from the beginning because I knew that we would have
multiple implementations to test on the same test cases.

The function `test_interpreter` is not very interesting (it's a for loop with a
function call) so I have omitted it in the interest of brevity. Feel free to
check out the implementation [in the repo](https://github.com/tekknolagi/ddcg).

```c++
int main() {
  // ...
  fprintf(stderr, "Testing interpreter (expr) ");
  test_interpreter<Interpreter>(expr_tests);
  fprintf(stderr, "Testing interpreter (stmt) ");
  test_interpreter<Interpreter>(stmt_tests);
}
```

And that's it. If all went well on your end, you should see something like this
in your terminal:

```
$ ./interp
Testing interpreter (expr) .......
Testing interpreter (stmt) ...........
$
```

This indicates that we have laid a good base upon which we can experiment.

## Compiler

Ahhh, finally onto the real meat of the post: the compiler! We'll follow the
talk, meaning that we'll look at three different compilers:

* the baseline compiler, with very simple code generation
* the destination-driven compiler
* the destination-driven compiler *with control destinations*

That way we can compare results across the board. We won't look at performance
(see [my commentary](/inline-caching/#performance-analysis) on performance
analysis, which I should probably turn into a separate post). Instead we will
look at one or two assembly listings across the different implementations and
note the differences.

To organize the implementations, we have an abstract JIT that does a lot of the
assembler and code management. It also has some utility functions like `varAt`
that put together the memory addressing mode for a given variable index.

```c++
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

  Address varAt(word index) {
    return Address(RDI, index * sizeof(word));
  }

  virtual void compileExpr(const Expr* expr) = 0;
  virtual void compileStmt(const Stmt* stmt) = 0;
  // ...
};
```

`compileExpr` and `compileStmt` are left as exercises for the subclasses.

All of the implementations will be using a version of the [Dart][dart-sdk]
assembler that was ported for the [Skybison][skybison] project a couple of
years ago. It's nice because it's only a couple of files but provides a C++-y
x86_64 assembler interface.

[dart-sdk]: https://github.com/dart-lang/sdk
[skybison]: https://github.com/tekknolagi/skybison

Assembling instructions from C++ will look like:

```c++
void emitAdd(Assembler* as) {
  __ addq(RAX, RCX);
}
```

Where `__` is a shorthand for `as.` to avoid visual clutter.

### Baseline

Now that we've expressed our desired behavior in the form of a test suite, we
can change the implementation and maybe see if we've broken things. When I was
writing the compiler, I broke a number of tests. It was really valuable to have
a bunch of tests that exercise different combinations of expression types,
since those can isolate compiler bugs.

We'll start off with the baseline compiler, which emits very straightforward
context-unaware code. Compiling `AddExpr`, for example, uses the stack heavily:

* compile left hand side, store on stack
* compile right hand side, store on stack
* pop from stack
* add
* store result on stack

It's very convenient to be able to treat the hardware as a stack machine. I
think I got this codegen right the first time, which is *very* different from
my experience with the other techniques.

```c++
class BaselineJIT : public JIT {
 public:
  virtual void compileExpr(const Expr* expr) {
    compileExprHelper(expr);
    // Return the value from the top of the stack.
    __ popq(RAX);
  }

  void compileExprHelper(const Expr* expr) {
    // RCX is caller-saved, meaning that callees (our code) can do whatever we
    // want with it.
    Register tmp = RCX;
    switch (expr->type) {
      case ExprType::kIntLit: {
        word value = static_cast<const IntLit*>(expr)->value;
        __ pushq(Immediate(value));
        break;
      }
      case ExprType::kAddExpr: {
        auto add = static_cast<const AddExpr*>(expr);
        compileExprHelper(add->left);
        compileExprHelper(add->right);
        __ popq(tmp);
        __ popq(RAX);
        __ addq(RAX, tmp);
        __ pushq(RAX);
        break;
      }
      case ExprType::kVarRef: {
        int offset = static_cast<const VarRef*>(expr)->offset;
        __ pushq(varAt(offset));
        break;
      }
      case ExprType::kVarAssign: {
        auto assign = static_cast<const VarAssign*>(expr);
        compileExprHelper(assign->right);
        // Leave the value on the stack for assignment chains like a = b = 1
        __ movq(RAX, Address(RSP, 0));
        __ movq(varAt(assign->left->offset), RAX);
        break;
      }
      case ExprType::kLessThan: {
        auto less = static_cast<const LessThan*>(expr);
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
  // ...
};
```

Let's look at a sample bit of code generated for adding two numbers:

```c++
Expr* expr = new AddExpr(new IntLit(123), new IntLit(456));
BaselineJIT jit;
State state;
jit.interpret(&state, expr);
jit.dis();
fprintf(stderr, "code size: %ld bytes\n", jit.codeSize());
// 0x621000057900    55                     push rbp
// 0x621000057901    4889e5                 movq rbp,rsp
// 0x621000057904    6a7b                   push 0X7B
// 0x621000057906    68c8010000             push 0X1C8
// 0x62100005790B    59                     pop rcx
// 0x62100005790C    58                     pop rax
// 0x62100005790D    4803c1                 addq rax,rcx
// 0x621000057910    50                     push rax
// 0x621000057911    58                     pop rax
// 0x621000057912    4889ec                 movq rsp,rbp
// 0x621000057915    5d                     pop rbp
// 0x621000057916    c3                     ret
// code size: 23 bytes
```

See all of the stack motion? We push `0x7B` and `0x1C8` only to immediately pop
them again---and the same thing with the result. It's all a bit silly. If I
were writing this by hand, I would probably write something like:

```
mov rax, 123
mov rcx, 456
add rax, rcx
```

Now let's look at some code generated for an `IfStmt` with a `LessThan`
operator:

```c++
Stmt* stmt = new IfStmt(new LessThan(new IntLit(1), new IntLit(2)),
                new ExprStmt(new VarAssign(new VarRef(0), new IntLit(123))),
                new ExprStmt(new VarAssign(new VarRef(0), new IntLit(456))));
BaselineJIT jit;
State state;
jit.interpret(&state, stmt);
jit.dis();
fprintf(stderr, "code size: %ld bytes\n", jit.codeSize());
// 0x621000057900    55                     push rbp
// 0x621000057901    4889e5                 movq rbp,rsp
// 0x621000057904    6a01                   push 1
// 0x621000057906    6a02                   push 2
// 0x621000057908    59                     pop rcx
// 0x621000057909    58                     pop rax
// 0x62100005790A    482bc1                 subq rax,rcx
// 0x62100005790D    b800000000             movl rax,0
// 0x621000057912    0f9cc0                 setll rax
// 0x621000057915    50                     push rax
// 0x621000057916    58                     pop rax
// 0x621000057917    4885c0                 testq rax,rax
// 0x62100005791A    740c                   jz 0X0000621000057928
// 0x62100005791C    6a7b                   push 0X7B
// 0x62100005791E    488b0424               movq rax,[rsp]
// 0x621000057922    488907                 movq [rdi],rax
// 0x621000057925    58                     pop rax
// 0x621000057926    eb0d                   jmp 0X0000621000057935
// 0x621000057928    68c8010000             push 0X1C8
// 0x62100005792D    488b0424               movq rax,[rsp]
// 0x621000057931    488907                 movq [rdi],rax
// 0x621000057934    58                     pop rax
// 0x621000057935    4889ec                 movq rsp,rbp
// 0x621000057938    5d                     pop rbp
// 0x621000057939    c3                     ret
// code size: 58 bytes
```

This code is also not great. In fact, it's positively enormous! Not only do we
push and pop our inputs to the stack, but we also materialize the result of the
comparison (`subq`) and then test *that* (`testq`) for the `IfStmt`. There is
no need to do this: we should instead be able to use the flags already set by
the `subq`.

Let's try to figure out how to improve it.

### Destination-driven

```c++
Expr* expr = new AddExpr(new IntLit(123), new IntLit(456));
DestinationDrivenJIT jit;
State state;
jit.interpret(&state, expr);
jit.dis();
fprintf(stderr, "code size: %ld bytes\n", jit.codeSize());
// 0x621000057900    55                     push rbp
// 0x621000057901    4889e5                 movq rbp,rsp
// 0x621000057904    6a7b                   push 0X7B
// 0x621000057906    b8c8010000             movl rax,0X1C8
// 0x62100005790B    59                     pop rcx
// 0x62100005790C    4803c1                 addq rax,rcx
// 0x62100005790F    4889ec                 movq rsp,rbp
// 0x621000057912    5d                     pop rbp
// 0x621000057913    c3                     ret
// code size: 20 bytes
```

If you compare with the baseline code, you can see right off the bat that there
is less stack activity: yes, we push `0x7B`, but we move `0x1C8` into a
register, `RCX`. Then we don't push and pop the result! We just keep it in
`RAX`. A huge improvement.

### Add control destinations

## Conclusion


<br />
<hr style="width: 100px;" />
<!-- Footnotes -->
