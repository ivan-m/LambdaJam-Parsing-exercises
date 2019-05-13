{-# LANGUAGE BangPatterns #-}

{- |
   Module      : Parsers.Commit
   Description : Commitment-based parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

   Exercise 2: Implement a parser with 'commit'.

   Quite often you know that you've reached a point in your parser
   where backtracking should /not/ be allowed (i.e. a parsing error
   from this point on indicates that any error should be considered
   bad\/invalid input).

   Consider which functions might make sense to contain 'commit' in
   them (should /any/ contain 'commit'? does it reduce their
   flexibility? remember that you may have overlapping patterns with
   '(<|>)'!).

   Once you're done, use this in "Regex.CommitParser".

 -}
module Parsers.Commit
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

-- | The result of a parser: either an error, the expected result or a
--   \"commitment\" that backtracking is not allowed (along with the
--   remaining unconsumed input @z@).
data Result z a = Err z String
                | OK  z a
                | Commit (Result z a)
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
mapResult f (Commit r)  = addCommit (mapResult f r)

-- | Ensure that the outermost constructor is 'Commit'.
addCommit :: Result z a -> Result z a
addCommit c@Commit{} = c
addCommit r          = Commit r

-- | The data structure for our parser.
--
--   We can run our parser with 'runParser'.
newtype Parser a = P { runParser :: String -> Result String a }

-- | As with 'runParser', but returns a 'Maybe' rather than a 'Result'
--   (to avoid dealing with a parser-specific result type when
--   comparing parsers).  We also ensure all input was consumed.
runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe p = fromResult . runParser p
  where
    fromResult (Commit r) = fromResult r
    fromResult (OK "" a)  = Just a
    fromResult _          = Nothing

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
apply pf pa = P $ go . runParser pf
  where
    go (OK str f)    = runParser (f <$> pa) str
    go (Err str err) = Err str err
    go (Commit r)    = addCommit (go r)

{-

Alternatively:

apply pf pa = do
  f <- pf
  a <- pa
  pure (f a)

-}

infixl 3 `apply`

discard :: Parser a -> Parser b -> Parser a
discard pa pb = P $ evalA . runParser pa
  where
    evalA (OK str a) = evalB a (runParser pb str)
    evalA (Commit r) = addCommit (evalA r)
    evalA err        = err

    evalB a = go
      where
        go (OK str !_)   = OK str a
        go (Err str err) = Err str err
        go (Commit !r)   = addCommit (go r)

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

-- | As with 'failParser' but indicates something has gone severely
-- wrong and you shouldn't be able to backtrack from here.
failParserBad :: String -> Parser a
failParserBad = commit . failParser

-- | Specify the error message for when a parser fails, and don't
--   backtrack.
(<?!>) :: Parser a -> String -> Parser a
pa <?!> err = commit (pa <?> err)

-- Make sure that if the result of @pa@ is @Commit (Err str err)@ that
-- you don't backtrack!
onFail :: Parser a -> Parser a -> Parser a
onFail pa pb = P $ \str -> case runParser pa str of
                             Err{} -> runParser pb str
                             -- This will handle the Commit case as
                             -- well.  The presence of Commit "masks"
                             -- any error from this function.
                             r     -> r

instance Monad Parser where
  return = pure

  (>>=) = withResult

  -- You should use 'failParser' explicitly.
  fail = failParser

withResult :: Parser a -> (a -> Parser b) -> Parser b
withResult pa f = P $ go . runParser pa
  where
    go (OK str a)    = runParser (f a) str
    go (Err str err) = Err str err
    go (Commit r)    = addCommit (go r)

--------------------------------------------------------------------------------

-- | Mark that no backtracking from this value should occur.
--
--   Consider the case of @commit (commit p)@; what should the result
--   of this be?
commit :: Parser a -> Parser a
commit p = P $ addCommit . runParser p

-- | Succeeds only if there's no more input.
endOfInput :: Parser ()
endOfInput = P $ \str -> case str of
                           "" -> OK  str ()
                           _  -> Err str "Unconsumed input"

-- | Returns the next character; fails if none exists.
next :: Parser Char
next = P $ \str -> case str of
                     (c:str') -> OK  str' c
                     _        -> Err str  "No input remaining"

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
-- You may be tempted to have a `commit` in here, but you may have
-- cases of overlapping patterns.  As such, the usual recommendation
-- is that the caller should consider using 'commit' on the closing
-- @ket@ case.
