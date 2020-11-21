# Haskell c4f Experiment

This is a Haskell toy project looking at implementing a clone of the
[C4 (né ACT) fuzzer](https://github.com/MattWindsor91/act) in Haskell.

*NOTE:* I _don't_ work on this in any official or serious capacity.
If you're expecting a fuzzer that _actually works_, is being properly and
actively developed, and has a realistic prospect of going anywhere any time
soon, check the above.

## Why this project exists

- Fun, mostly; it's a nice warm-up to play around with on mornings, and
  occasional evenings and weekends.
- Trying to relearn Haskell, and I find it a lot easier to pick things up
  or back up if they have motivation.
- If I see any obviously better way to implement bits of C4 while I'm messing
  around with c4f-hs, I'll see if I can get them backported.
- One of the most annoying things I've noticed while developing C4 is that
  I constantly reach for monads, typeclass surrogates, and laziness - so I
  thought it'd be interesting to see if a language that works well with those
  concepts rather than fighting against OCaml.

## Licence

MIT; at present, unlike the OCaml fuzzer, this project has not inherited any
code from otherwise-licenced projects.

## What it can do

Nothing, at the moment: it just has parts of the expression evaluator, operator
algebra, and internal representation AST so far.  It'll be a long time (tends
to infinity) before it can actually fuzz things.
