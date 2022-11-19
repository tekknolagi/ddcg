---
title: Destination-driven code generation
---

I saw Phil Eaton post about this paper, [Destination-Driven Code
Generation][ddcg-paper] (PDF) on Twitter. He posted about how the V8 team used
it in the early days to do really fast but still half-decent codegen for their
compiler. It sounded really interesting, so I skimmed the paper, saw a lot of
greek symbols, and bailed out. Then I saw that Phil had posted [a
talk][ddcg-talk] (PDF) by a V8 engineer that laid out the paper's core ideas
really clearly. To the paper's credit, the actual text is extremely clear. I
think I just wasn't ready for Greek letters that day.

[ddcg-paper]: https://legacy.cs.indiana.edu/~dyb/pubs/ddcg.pdf
[ddcg-talk]: https://raw.githubusercontent.com/eatonphil/one-pass-code-generation-in-v8/main/One-pass%20Code%20Generation%20in%20V8.pdf

In any case, while V8 ostensibly used this strategy in their early codegen, I
couldn't find their implementation, nor could I find any other
implementation[^jsc] of this paper. So I decided to write one!

[^jsc]: I was writing this post, I discovered Phil's
    [implementation][https://github.com/eatonphil/jsc] of this paper in his
    ahead-of-time JavaScript compiler, *jsc* that compiles JS to Node C-API
    calls. Note that this is a different `jsc` than Apple's JavaScriptCore, the
    runtime that powers WebKit.

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

x

<br />
<hr style="width: 100px;" />
<!-- Footnotes -->
