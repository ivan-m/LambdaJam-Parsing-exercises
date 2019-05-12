{-# LANGUAGE BangPatterns #-}

{- |
   Module      : Parsers.Simple
   Description : A basic parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

  This simple parser demonstrates the basics about how parser
  combinators can be written.  See the module "Regex.SimpleParser" for
  an example of how to use it.

 -}
module Parsers.Simple
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

instance Functor (Result z) where
  fmap = mapResult

{-

Alternatively, GHC can auto-derive Functor using the DeriveFunctor
LANGUAGE pragma.

-}

mapResult :: (a -> b) -> Result z a -> Result z b
mapResult f (OK z a)    = OK z (f a)
mapResult _ (Err z err) = Err z err

-- | The data structure for our parser.
--
--   We can run our parser with 'runParser'.
newtype Parser a = P { runParser :: String -> Result String a }

-- | As with 'runParser', but returns a 'Maybe' rather than a 'Result'
--   (to avoid dealing with a parser-specific result type when
--   comparing parsers).  We also ensure all input was consumed.
runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe p inp = case runParser p inp of
                         OK "" a -> Just a
                         _       -> Nothing

instance Functor Parser where
  fmap = mapParser

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = P $ \str -> f <$> runParser p str

instance Applicative Parser where
  pure = liftParser

  (<*>) = apply

  (<*) = discard

liftParser :: a -> Parser a
liftParser a = P $ \str -> OK str a

apply :: Parser (a -> b) -> Parser a -> Parser b
apply pf pa = P $ \str -> case runParser pf str of
                            OK  str' f   -> runParser (f <$> pa) str'
                            Err str' err -> Err str' err

{-

Alternatively:

apply pf pa = do
  f <- pf
  a <- pa
  pure (f a)

-}

infixl 3 `apply`

discard :: Parser a -> Parser b -> Parser a
discard pa pb = P $ \str -> case runParser pa str of
                              OK str' a -> case runParser pb str' of
                                             -- ! to make sure the parser evaluates
                                             OK str'' !_   -> OK str'' a
                                             Err str'' err -> Err str'' err
                              err       -> err

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
failParser err = P $ \str -> Err str err

-- | Specify the error message for when a parser fails.
(<?>) :: Parser a -> String -> Parser a
pa <?> err = pa <|> failParser err

onFail :: Parser a -> Parser a -> Parser a
onFail pa pb = P $ \str -> case runParser pa str of
                             Err{} -> runParser pb str
                             ok    -> ok

instance Monad Parser where
  return = pure

  (>>=) = withResult

  -- You should use 'failParser' explicitly.
  fail = failParser

withResult :: Parser a -> (a -> Parser b) -> Parser b
withResult pa f = P $ \str -> case runParser pa str of
                                OK  str' a   -> runParser (f a) str'
                                Err str' err -> Err str' err

--------------------------------------------------------------------------------

-- | Succeeds only if there's no more input.
endOfInput :: Parser ()
endOfInput = P $ \str -> case str of
                           "" -> OK  str ()
                           _  -> Err str "Unconsumed input"

-- | Returns the next character; fails if none exists.
next :: Parser Char
next = P $ \str -> case str of
                     (c:str') -> OK  str' c
                     _        -> Err str  "No input remaining."

{-

Alternatively

next = satisfy (const True)

-}

-- | Succeeds only if the next character satisfies the provided
--   predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- next
  if p c
    then pure c
    else failParser "satisfy failed"

-- | Parse the specified character.
char :: Char -> Parser Char
char c = satisfy (==c)

-- | Returns the result of the first parser that succeeds.
oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) noneSucceed
  where
    noneSucceed  = failParser "Failed to parse any of the possible choices."

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- | As with 'sepBy' but return a non-empty list.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- | Parses the elements between the @bra@ and @ket@ parsers.
bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket bra ket pa = bra *> pa <* ket
