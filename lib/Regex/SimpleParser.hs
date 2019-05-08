{- |
   Module      : Regex.SimpleParser
   Description : Basic parsers for Regexes
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

   Example demonstration of how to use parser combinators and the
   first exercise.

 -}
module Regex.SimpleParser
  ( parseRegex
  , applyRegex
  ) where

import Parsers.Simple
import Regex.Types

import Control.Applicative
import Control.Monad       (void)
import Data.Char           (isAlphaNum)
import Data.Maybe          (isJust)

--------------------------------------------------------------------------------

-- | Try to parse a regular expression.
parseRegex :: String -> Maybe Pattern
parseRegex = runParserMaybe (parsePattern <* endOfInput)
-- We want to make sure the parser reads the entire input; however, we
-- don't put this in 'parsePattern' due to the recursive call.

-- Because 'ConcatenatedAtoms' are allowed to be empty, parsing it
-- will always succeed.  As such, there is no need for the extra test
-- in 'sepBy'.
parsePattern :: Parser Pattern
parsePattern = Pattern <$> sepBy1 parseConcatenatedAtoms (char '|')

-- We need to allow the empty case as that's valid!
parseConcatenatedAtoms :: Parser ConcatenatedAtoms
parseConcatenatedAtoms = ConcatenatedAtoms <$> many parseQuantifiedAtom

-- '(<**>)' is the same as '(<*>)' but in the opposite order.
parseQuantifiedAtom :: Parser QuantifiedAtom
parseQuantifiedAtom = parseAtom <**> parseQuantifier

{-

Alternatively:

parseQuantifiedAtom = do
  atom  <- parseAtom
  quant <- parseQuantifier
  pure (quant atom)

-}

-- | Parse an individual atom.
parseAtom :: Parser Atom
parseAtom = oneOf [ char '.' *> pure AnyChar
                  , SpecificChar <$> parseCharacter
                  , BExpression  <$> parseBracketExpression
                  , SubPattern   <$> bracket (char '(') (char ')') parsePattern
                  ]

parseQuantifier :: Parser (Atom -> QuantifiedAtom)
parseQuantifier = oneOf [ char '?' *> pure OptionalAtom
                        , char '+' *> pure AtLeastOne
                        , char '*' *> pure Multiple
                        ,             pure PlainAtom -- Needs to be last.
                        ]

-- | Parses a non-meta character, or an escaped meta-character.
parseCharacter :: Parser Char
parseCharacter = satisfy (`notElem`metaChars)
                 <|> (char '\\' *> satisfy (`elem`metaChars))

parseBracketExpression :: Parser BracketExpression
parseBracketExpression = bracket (char '[') (char ']')
                         $ BracketExpression <$> checkInverse <*> some parseBracketPattern
  where
    -- Check if the first character is ^
    checkInverse = isJust <$> optional (char '^')

-- | Parses a single character or a character range, excluding the
--   special cases of @]@ and @-@.
parseBracketPattern :: Parser BracketPattern
parseBracketPattern = do
  c <- parseBracketChar
  oneOf [ char '-' *> fmap (BracketRange c) parseBracketChar
        , pure (BracketChar c)
        ]
  where
    parseBracketChar = satisfy isAlphaNum

{-

Alternatively:

parseBracketExpression = do
  -- Make sure you get the order right!
  oneOf [ BracketRange <$> parseBracketChar <*> (char '-' *> parseBracketChar)
        , BracketChar <$> parseBracketChar
        ]
  where
    parseBracketChar = satisfy isAlphaNum

-}

--------------------------------------------------------------------------------

{-

Exercise 1: take a 'Pattern' and create a 'Parser' out of it.

We only want to use this to /validate/ a 'String' using a regular
expression (i.e. check if the 'String' is matched by the 'Pattern').
As such, the return values from the 'Parser's don't matter.  As such,
the @void@ function from "Control.Monad" may be helpful.  Specialised,
it becomes:

> void :: Parser a -> Parser ()

That is, we ignore the parsing result.

Try testing out 'applyRegex' in ghci.

You can also help test your implementation with the test seat (see
@README.md@ on how to run the test-suite).

-}

-- | Take a 'String' containing a regular expression and a second
--   'String'; test if the regular expression satisfies the regular
--   expression.
applyRegex :: String -> String -> Bool
applyRegex = error "undefined: applyRegex"

-- | Test if the 'String' satisfies the provided 'Pattern'.
--
--   Consider ensuring that the 'Pattern' consumes the entire 'String'
--   (but make sure this isn't the case for 'regexToParser' due to the
--   recursion!).
--
--   To test if the parser was successful, consider:
--
--   > isJust :: Maybe a -> Bool
satisfiesRegex :: Pattern -> String -> Bool
satisfiesRegex = error "undefined: satisfiesRegex"

-- | Convert the supplied regular expression into a parser.  All we
--   care is if the regex is satisfied, not the actual result.
regexToParser :: Pattern -> Parser ()
regexToParser = error "undefined: regexToParser"

-- | There is another handy function available that you may wish to
--   consider; specialised, it becomes:
--
--   > mapM_ :: (a -> Parser ()) -> [a] -> Parser ()
--
--   This is similar to 'map', but only cares about /performing/ the
--   actions, not the results of them.  It has the effect of running
--   every specified parser in turn.
concatenatedAtomsToParser :: ConcatenatedAtoms -> Parser ()
concatenatedAtomsToParser = error "undefined: concatenatedAtomsToParser"

-- | You may wish to take use these functions (all from
--   "Control.Applicative"):
--
--   > optional :: Parser a -> Parser (Maybe a)
--
--   > some :: Parser a -> Parser [a]
--
--   > many :: Parser a -> Parser [a]
quantifiedAtomToParser :: QuantifiedAtom -> Parser ()
quantifiedAtomToParser = error "undefined: quantifiedAtomToParser"

atomToParser :: Atom -> Parser ()
atomToParser = error "undefined: atomToParser"

bracketExpressionToParser :: BracketExpression -> Parser ()
bracketExpressionToParser = error "undefined: bracketExpressionToParser"

-- | Create the predicate that can be provided to 'satisfy'.  We want
--   to know if a 'Char' matches a 'BracketExpression'.
--
--   Don't forget to take into account the 'inverseExpression' field!
--   Remember, if it's 'True', then we wish /none/ of the
--   'BracketPattern's to match.
--
--   You may find this function helpful:
--
--   > any :: (a -> Bool) -> [a] -> Bool
--
--   This returns 'True' if the predicate succeeds on /any/ of the
--   provided elements.
satisfiesBracketExpression :: BracketExpression -> (Char -> Bool)
satisfiesBracketExpression  = error "undefined: satisfiesBracketExpression "

-- | Check if a character is matched by the 'BracketPattern'.
satisfiesBracketPattern :: Char -> BracketPattern -> Bool
satisfiesBracketPattern = error "undefined: satisfiesBracketPattern"
