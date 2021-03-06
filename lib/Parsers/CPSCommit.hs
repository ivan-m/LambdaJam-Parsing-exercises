{-# LANGUAGE RankNTypes #-}

{- |
   Module      : Parsers.CPSCommit
   Description : A function-based parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

  Exercise 4: Add 'commit' to "Parsers.CPS".

  Merge together "Parses.Commit" and "Parsers.CPS".  This is tricky
  though as whereas before commitment was part of the result, now it's
  part of the /state/.

  As such, use the 'withoutCommitment' function to implement 'onFail'
  and 'oneOf'.

 -}
module Parsers.CPSCommit
  ( -- * Parser definition
    Parser
  , Result (..)
  , runParser
  , runParserMaybe
    -- * Parser combinators
  , commit
  , failParser
  , (<?>)
  , failParserBad
  , (<?!>)
  , endOfInput
  , next
  , satisfy
  , char
  , oneOf
  , sepBy
  , sepBy1
  , bracket
  ) where

import Control.Applicative

--------------------------------------------------------------------------------

-- | The result of a parser: either an error or the expected result
--   (along with the remaining unconsumed input @z@).
--
--   This is equivalent to @(z, 'Either' String a0@, but we use a
--   new data type that fuses these together to provide better
--   safety\/emphasis.
data Result z a = Err z String
                | OK  z a
  deriving (Eq, Ord, Show, Read)

-- | The data structure for our parser.
--
--   We can run our parser with 'runParser'.
newtype Parser a = P {
  -- Our parser is actually a function of functions!
  runP :: forall r.
          String
          -> Commitment         -- Whether we can backtrack.
          -> Failure          r -- What to do when we fail.
          -> Success        a r -- What to do when we succeed.
          -> Result  String   r
  }

{-

How the parser works
--------------------

Traditionally, the way a parser works has been "bottom-up": a
sub-parser is evaluated, returning a @Result@ value; if it fails do
one thing, if it succeeds do another.

Instead, for this parser the caller specifies what the sub-parser
should do if it reaches a failure or when it succeeds.

Since function composition/calling is more efficient than continually
doing comparisons on data types, this results in _speed_!

In terms of usage, this is completely opaque to the user of the
parsing library: they are not specifically asked for any functions for
dealing with success or failure, etc.

Constructing a low-level parser typically looks like:

> ... = P $ \ inp cm fl sc -> ...
              |   |  |  |
              |   |  |  |
              |   |  |  \-- What to do on a successful parse.
              |   |  |
              |   |  \----- What to do if the parse isn't successful.
              |   |
              |   \-------- Whether we are allowed to backtrack.
              |
              \------------ Input to the parser.

Joining two parsers is typically a matter of replacing the @sc@ call
with the new parser.  See 'mapParser' for an example.

-}

-- | We define a new data structure to avoid boolean blindness:
--   https://shreevatsa.wordpress.com/2015/01/31/boolean-blindness/
--
--   In other words, we don't want to have to remember whether 'True'
--   means \"committed\" or \"can backtrack\".
data Commitment = UnCommitted | Committed
  deriving (Eq, Ord, Show, Read)

-- | How to combine two 'Commitment' values.  We want to make sure
--   that once we're committed we /stay/ committed.
instance Semigroup Commitment where
  Committed <> _ = Committed
  _         <> c = c

-- What to do when we fail a parse.  Takes in the remaining input and
-- an error message.
type Failure   r = String -> Commitment -> String -> Result String r

-- What to do when a parse is successful.
type Success a r = String -> Commitment -> a      -> Result String r

-- Default Failure function.
failure :: Failure   r
failure = error "Undefined: failure"

-- Default Success implementation.
successful :: Success a a
successful = error "Undefined: successful"

-- | Run the actual parser
--
--   We provide the default 'Failure' and 'Success' implementations,
--   which should eventually be passed down to the actual
--   success\/failure calls.
runParser :: Parser a -> String -> Result String a
runParser = error "Undefined: runParser"

-- | As with 'runParser', but returns a 'Maybe' rather than a 'Result'
--   (to avoid dealing with a parser-specific result type when
--   comparing parsers).  We also ensure all input was consumed.
runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe = error "Undefined: runParserMaybe"

instance Functor Parser where
  fmap = mapParser

-- We replace the 'Success' result with a call that applies the
-- provided function to the result.
--
-- If you squint, it's analogous to how this function was defined in
-- "Parsers.Simple":
--
--  * For the failure case, leave it as-is.
--
--  * For the success case, apply the function and wrap it back up
--  * again.
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = P $ \str cm fl sc ->
  runP p str cm fl $ \str' cm' a -> sc str' cm' (f a)

instance Applicative Parser where
  pure = liftParser

  (<*>) = apply

  (<*) = discard

liftParser :: a -> Parser a
liftParser = error "Undefined: liftParser"

apply :: Parser (a -> b) -> Parser a -> Parser b
apply = error "Undefined: apply"

infixl 3 `apply`

discard :: Parser a -> Parser b -> Parser a
discard = error "Undefined: discard"

infixl 3 `discard`

instance Alternative Parser where
  empty = failParser "empty"

  (<|>) = onFail

  -- 'many' and 'some' are often defined in this way to optimise
  -- performance.

  many p = many_v
    where
      many_v = some_v <|> pure []

      some_v = (:) <$> p <*> many_v

  some p = some_v
    where
      many_v = some_v <|> pure []

      some_v = (:) <$> p <*> many_v

-- | Whilst this is also available as 'fail', you may wish to use this
--   explicitly to be clear with your intentions.
failParser :: String -> Parser a
failParser = error "Undefined: failParser"

-- | Specify the error message for when a parser fails.
(<?>) :: Parser a -> String -> Parser a
(<?>) = error "Undefined: (<?>)"

-- | As with 'failParser' but indicates something has gone severely
-- wrong and you shouldn't be able to backtrack from here.
failParserBad :: String -> Parser a
failParserBad = error "Undefined: failParserBad"

-- | Specify the error message for when a parser fails, and don't
--   backtrack.
(<?!>) :: Parser a -> String -> Parser a
(<?!>) = error "Undefined: (<?!>)"

-- Helper function for handling commitment: run the specified parser
-- 'UnCommitted'.
--
-- With 'onFail' and 'oneOf', we want to make sure that we don't
-- consider the commitment of the /parent/ for knowing whether to
-- backtrack the /children/.
withoutCommitment :: Parser a -> Parser a
withoutCommitment = error "Undefined: withoutCommitment"

onFail :: Parser a -> Parser a -> Parser a
onFail = error "Undefined: onFail"

-- Assume that the original commitment value was 'UnCommitted'.
onFailWith :: Parser a -> Parser a -> Parser a
onFailWith = error "Undefined: onFailWith"

instance Monad Parser where
  return = pure

  (>>=) = withResult

  -- You should use 'failParser' explicitly.
  fail = failParser

withResult :: Parser a -> (a -> Parser b) -> Parser b
withResult = error "Undefined: withResult"

--------------------------------------------------------------------------------

-- | Mark that no backtracking from this value should occur.
--
--   Consider the case of @commit (commit p)@; what should the result
--   of this be?
commit :: Parser a -> Parser a
commit = error "Undefined: commit"

-- | Succeeds only if there's no more input.
endOfInput :: Parser ()
endOfInput = error "Undefined: endOfInput"

-- | Returns the next character; fails if none exists.
next :: Parser Char
next = error "Undefined: next"

-- | Succeeds only if the next character satisfies the provided
--   predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy = error "Undefined: satisfy"

-- | Parse the specified character.
char :: Char -> Parser Char
char = error "Undefined: char"

-- | Returns the result of the first parser that succeeds.
oneOf :: [Parser a] -> Parser a
oneOf = error "Undefined: oneOf"

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy = error "Undefined: sepBy"

-- | As with 'sepBy' but return a non-empty list.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 = error "Undefined: sepBy1"

-- | Parses the elements between the @bra@ and @ket@ parsers.
bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket = error "Undefined: bracket"
