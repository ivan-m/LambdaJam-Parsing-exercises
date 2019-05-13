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

You should probably do an initial build of the this repository before
coming to the workshop to make sure you've downloaded all
dependencies.

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
above).  You can also try running the benchmarks; can you understand
why successful parsers are faster than failing ones?

Note: this is _not_ the most efficient way of running regular
expressions (and doesn't match the expected behaviour of many of
them - see the note in [`regex-tester.hs`] regarding the email regular
expression).

### Exercise 2

In the [slides] the notion of "commitment" is briefly mentioned.  In
this exercise you will make use of it.

The purpose of commitment is to indicate that you know you are deep
enough in a parse that you have chosen the correct branch.  As such,
if a parsing failure occurs, you shouldn't take any other branch when
choosing between multiple parsers.  In the case of your parser
succeeding this has no real impact; in the case of invalid input it
can lead to more specific error messages and - if there are a large
number of partially overlapping choices to choose from - faster
parsing.

(However, `commit` tends not to do very well when combined with
functions like `many` or `some`; can you see why?)

To start with, you will want to implement the new parser in
[`Commit.hs`](lib/Parsers/Commit.hs).  Consider where it makes sense
to add in default `commit` calls (it may very well be "nowhere").

After that, use this new parser to port over the parsers in
[`SimpleParser.hs`] to
[`CommitParser.hs`](lib/Regex/SommitParser.hs).  Where should you use
`commit` here?

(You should hopefully note that many of the combinators and parsers
are identical: once you start using the abstractions you don't need to
worry about the underlying implementation!)

Test out the differences in how the parser (using `runParser` to get
error messages) performs; for example, try to parse `"([ab)"`.

Test your new parsers by changing which parser is un-commented in
[`regex-tester.hs`] and run the test suite.  Once it succeeds,
uncomment the `CommitParser` lines in [`regex-comparison.hs`] and see
how it compares to the original parser (it's hopefully faster to fail
in parsers that don't succeed).

(You probably won't get much of a performance improvement due to the
small number of backtracking options.  In fact, performance might be
_worse_: can you see why?)

For more information on the justification behind commitment you can
read [this
paper](https://www.cs.york.ac.uk/plasma/publications/pdf/partialparse.pdf)
by Malcolm Wallace, author of
[polyparse](https://hackage.haskell.org/package/polyparse) (that these
parsers are heavily based on).

### Exercise 3

Hopefully you would have intuitied with the previous two exercises
that a major slow down in the implementations of the previous two
parsers was that we had to keep stopping to check which `Result` value
we had (being even worse in the 2nd one as we had three cases instead
of just two).

GHC highly optimises functions and function composition; as such, we
can take advantage of this by using a _Continuation Passing Style_ (or
CPS) approach to implementing a parser.

The basic notion is that instead of checking the `Result` type:

```haskell
data Result z a = OK  z a
                | Err z String
```

and doing something different for each constructor, we carry around -
and modify _functions_ that represent what we would do with each of
these constructors.

Implement the parser in [`CPS.hs`](lib/Parsers/Commit.hs) and use
it in [`CPSParser.hs`].  As before, run the
testsuite and compare the performance.

[`CPSParser.hs`]: lib/Regex/CPSParser.hs

(This parser is a simplified version of what is implemented in
[attoparsec](http://hackage.haskell.org/package/attoparsec).)

You should notice two things:

* The peformance is better (maybe not by much because we're not
  parsing very large things, but still better)

* The contents of [`CPSParser.hs`] is identical to [`SimpleParser.hs`]
  (and even the higher-level combinators have identical definitions)!
