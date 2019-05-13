{-# LANGUAGE RankNTypes #-}

{- |
   Module      : Parsers.CPS
   Description : A function-based parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

  Exercise 3: Implement a CPS parser.

  The major impact affecting the performance of the first two parsers
  was having to check the value of the `Result` type all the time.
  That is, we're switching from using functions - which are highly
  optimised in GHC - to having to pattern match on functions.

  As such, a good way to improve the performance of our parser
  combinator library is to use Continuation Passing Style: that is, we
  represent the different cases with more functions rather than data
  structures.

 -}
module Parsers.CPS
  ( -- * Parser definition
    Parser
  , Result (..)
  , runParser
  , runParserMaybe
    -- * Parser combinators
  , failParser
  , (<?>)
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

> ... = P $ \ inp fl sc -> ...
              |   |  |
              |   |  |
              |   |  \-- What to do on a successful parse.
              |   |
              |   \----- What to do if the parse isn't successful.
              |
              \--------- Input to the parser.

Joining two parsers is typically a matter of replacing the @sc@ call
with the new parser.  See 'mapParser' for an example.

-}

-- What to do when we fail a parse.  Takes in the remaining input and
-- an error message.
type Failure   r = String -> String -> Result String r

-- What to do when a parse is successful.
type Success a r = String -> a      -> Result String r

-- Default Failure function.
failure :: Failure   r
failure = error "undefined: failure"

-- Default Success implementation.
successful :: Success a a
successful = error "undefined: successful"

-- | Run the actual parser
--
--   We provide the default 'Failure' and 'Success' implementations,
--   which should eventually be passed down to the actual
--   success\/failure calls.
runParser :: Parser a -> String -> Result String a
runParser p str = runP p str failure successful

-- | As with 'runParser', but returns a 'Maybe' rather than a 'Result'
--   (to avoid dealing with a parser-specific result type when
--   comparing parsers).  We also ensure all input was consumed.
runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe = error "undefined: runParserMaybe"

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
mapParser f p = P $ \str fl sc ->
  runP p str fl $ \str' a -> sc str' (f a)

instance Applicative Parser where
  pure = liftParser

  (<*>) = apply

  (<*) = discard

liftParser :: a -> Parser a
liftParser = error "undefined: liftParser"

apply :: Parser (a -> b) -> Parser a -> Parser b
apply = error "undefined: apply"

{-

Alternatively:

apply pf pa = do
  f <- pf
  a <- pa
  pure (f a)

-}

infixl 3 `apply`

discard :: Parser a -> Parser b -> Parser a
discard = error "undefined: discard"

{-

Alternatively

discard pa pb = do
  a  <- pa
  !_ <- pb
  pure a

-}

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
failParser = error "undefined: failParser"

-- | Specify the error message for when a parser fails.
(<?>) :: Parser a -> String -> Parser a
(<?>) = error "undefined: (<?>)"

-- The main trick here is that when the first parser fails, we
-- consider the second parser.
onFail :: Parser a -> Parser a -> Parser a
onFail = error "undefined: onFail"

instance Monad Parser where
  return = pure

  (>>=) = withResult

  -- You should use 'failParser' explicitly.
  fail = failParser

withResult :: Parser a -> (a -> Parser b) -> Parser b
withResult = error "undefined: withResult"

--------------------------------------------------------------------------------

-- | Succeeds only if there's no more input.
endOfInput :: Parser ()
endOfInput = error "undefined: endOfInput"

-- | Returns the next character; fails if none exists.
next :: Parser Char
next = error "undefined: next"

-- | Succeeds only if the next character satisfies the provided
--   predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy = error "undefined: satisfy"

-- | Parse the specified character.
char :: Char -> Parser Char
char = error "undefined: char"

-- | Returns the result of the first parser that succeeds.
oneOf :: [Parser a] -> Parser a
oneOf = error "undefined: oneOf"

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy = error "undefined: sepBy"

-- | As with 'sepBy' but return a non-empty list.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 = error "undefined: sepBy1"

-- | Parses the elements between the @bra@ and @ket@ parsers.
bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket = error "undefined: bracket"
