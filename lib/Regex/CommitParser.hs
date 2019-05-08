{- |
   Module      : Regex.CommitParser
   Description : Commitment-based parsers for Regexes
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

   Exercise 2: Port over "Regex.SimpleParser" over to using
   "Parsers.Commit".

   Consider where you may use the 'commit' function:

   * Where can you use it in 'parseRegex'?

   * Does it make sense to use it anywhere as part of 'regexToParser'?

   Once you're done, test it by un-commenting the appropriate line in
   @src/regex-tester.hs@ and compare it "Regex.SimpleParser" by
   uncommenting the lines in @src/regex-comparison.hs@.

 -}
module Regex.CommitParser
  ( parseRegex
  , applyRegex
  ) where

import Parsers.Commit
import Regex.Types

import Control.Applicative
import Control.Monad       (void)
import Data.Char           (isAlphaNum)
import Data.Maybe          (isJust)

--------------------------------------------------------------------------------

-- | Try to parse a regular expression.
parseRegex :: String -> Maybe Pattern
parseRegex = error "undefined: parseRegex"

parsePattern :: Parser Pattern
parsePattern = error "undefined: parsePattern"

parseConcatenatedAtoms :: Parser ConcatenatedAtoms
parseConcatenatedAtoms = error "undefined: parseConcatenatedAtoms"

parseQuantifiedAtom :: Parser QuantifiedAtom
parseQuantifiedAtom = error "undefined: parseQuantifiedAtom"

-- | Parse an individual atom.
parseAtom :: Parser Atom
parseAtom = error "undefined: parseAtom"

parseQuantifier :: Parser (Atom -> QuantifiedAtom)
parseQuantifier = error "undefined: parseQuantifier"

-- | Parses a non-meta character, or an escaped meta-character.
parseCharacter :: Parser Char
parseCharacter = error "undefined: parseCharacter"

parseBracketExpression :: Parser BracketExpression
parseBracketExpression = error "undefined: parseBracketExpression"

-- | Parses a single character or a character range, excluding the
--   special cases of @]@ and @-@.
parseBracketPattern :: Parser BracketPattern
parseBracketPattern = error "undefined: parseBracketPattern"

--------------------------------------------------------------------------------

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
