"Just Parsing Through"
======================

These are the exercises for the [Parsing Combinator
workshop](https://lambdajam.yowconference.com.au/proposal/?id=9695) at
the [2019 YOW! Lambda Jam](https://confengine.com/yow-lambda-jam-2019)
conference.

The [slides] from this workshop are also available.

[slides]: https://github.com/ivan-m/LambdaJam-Parsing

Sample solutions can be found in the `solutions` branch of this
repository.

Running these exercises
-----------------------

GHC 8.* is needed to build and run these exercises (if you are limited
to an older version of GHC you can either use Stack or remove the
benchmark section from [the cabal
file](LambdaJam-Parsing-exercises.cabal) and ignore any suggestions to
run them).

You have a few options on how to build and run the exercises:

1. `cabal-install`: I recommend you use the new Nix-style commands
   (for `cabal-install >= 2.4` you can use `v2-` prefixes instead of
   `new-`).

    You should at least prepare the required dependencies before
    attending the workshop with:

    ```bash
    cabal new-build --enable-tests --enable-benchmarks
    ```

    You can launch a REPL (for the library component) with:

    ```bash
    cabal new-repl lib:LambdaJam-Parsing-exercises
    ```

    Run the test-suite with `cabal new-test` and the benchmarks with
    `cabal new-bench`.

    (It is also possible to use the older sandbox-style method but I
    recommend against it.)

2. `stack`: prepare by running:

    ```haskell
    stack build --test --no-run-tests --bench --no-run-benchmarks
    ```

    The REPL can then be launched with:

    ```bash
    stack ghci LambdaJam-Parsing-exercises:lib
    ```

    `stack test` and `stack bench` will run the test-suite and
    benchmarks respectively.

Exercises
---------

The goal of the exercises is to practice using parser combinator
libraries and writing ones of your own to compare and contrast
implementations.

The file structure contains the different files required for all the
exercises:

1. The `lib/` directory contains the actual data structures and parser
   combinators.

    a) [`Simple.hs`](lib/Parsers/Simple.hs) contains a cleaned-up
       implementation of the Parser Combinator library developed in
       the [slides].

    b) [`Types.hs`](lib/Regex/Types.hs) contains the types found in
       the [slides] representing regular expressions.

    c) [`SimpleParser.hs`] uses the library to parse and construct the
       regular expression types (also found in the [slides]).

    The exercises are also found here.

2. The `src/` directory contains the test-suite and benchmarks.

    a) [`regex-tester.hs`] let you test an individual parser
       implementation.

    b) [`regex-comparison.hs`] allows comparing the performance of
       different parser implementations.

[`SimpleParser.hs`]: lib/Regex/SimpleParser.hs

[`regex-tester.hs`]: src/regex-tester.hs

[`regex-comparison.hs`]: src/regex-comparison.hs

### Exercise 1

Whilst the [slides] walked through _parsing_ a regular expression,
they didn't cover _running_ a regular expression.

In [`SimpleParser.hs`], implement `regexToParser` convert a regular
expression into a `Parser`.  From there, implementing `satisfiesRegex`
and `applyRegex` will allow you to see if a `String` is matched by a
regular expression.

You can ten test your implementation by running the test suite (see
above).

Note: this is _not_ the most efficient way of running regular
expressions (and doesn't match the expected behaviour of many of
them - see the note in [`regex-tester.hs`] regarding the email regular
expression).
