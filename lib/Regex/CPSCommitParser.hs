{- |
   Module      : Regex.CPSCommitParser
   Description : Commitment-based parsers for Regexes
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

   Exercise 3: Port over "Regex.CommitParser" over to using
   "Parsers.CPSCommit".

   Once you're done, test it by un-commenting the appropriate line in
   @src/regex-tester.hs@ and compare it "Regex.SimpleParser" by
   uncommenting the lines in @src/regex-comparison.hs@.

 -}
module Regex.CPSCommitParser
  ( parseRegex
  , applyRegex
  ) where

import Parsers.CPSCommit
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
                  , SubPattern   <$> parseSubPattern
                  ]
  where
    parseSubPattern = bracket (char '(') cls parsePattern
      where
        cls = char ')' <?!> "Invalid sub-pattern"

parseQuantifier :: Parser (Atom -> QuantifiedAtom)
parseQuantifier = oneOf [ char '?' *> pure OptionalAtom
                        , char '+' *> pure AtLeastOne
                        , char '*' *> pure Multiple
                        ,             pure PlainAtom -- Needs to be last.
                        ]

-- | Parses a non-meta character, or an escaped meta-character.
parseCharacter :: Parser Char
parseCharacter = satisfy (`notElem`metaChars)
                 <|> (char '\\' *> satisfy (`elem`metaChars) <?!> "Not a meta-character")

parseBracketExpression :: Parser BracketExpression
parseBracketExpression = bracket (char '[') cls
                         $ BracketExpression <$> checkInverse <*> some parseBracketPattern
  where
    -- Check if the first character is ^
    checkInverse = isJust <$> optional (char '^')

    cls = char ']' <?!> "Invalid bracket expression"

-- | Parses a single character or a character range, excluding the
--   special cases of @]@ and @-@.
parseBracketPattern :: Parser BracketPattern
parseBracketPattern = do
  c <- parseBracketChar
  commit $ oneOf [ char '-' *> fmap (BracketRange c) endRange
                 , pure (BracketChar c)
                 ]
  where
    parseBracketChar = satisfy isAlphaNum

    endRange = parseBracketChar <?!> "Invalid end-of-range character"

--------------------------------------------------------------------------------

{-

Note: we do /not/ use 'commit' here as we don't know how overlapping
all the different 'ConcatenatedAtoms' are.

-}

-- | Take a 'String' containing a regular expression and a second
--   'String'; test if the regular expression satisfies the regular
--   expression.
applyRegex :: String -> String -> Bool
applyRegex regex str = maybe False (`satisfiesRegex` str) (parseRegex regex)

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
satisfiesRegex regex = isJust . runParserMaybe prs
  where
    -- We want the parser to consume the entire input (only for the
    -- top-level parser, which is why we don't include this in
    -- 'regexToParser').
    prs = regexToParser regex <* endOfInput

-- | Convert the supplied regular expression into a parser.  All we
--   care is if the regex is satisfied, not the actual result.
regexToParser :: Pattern -> Parser ()
regexToParser (Pattern cas) = oneOf (map concatenatedAtomsToParser cas)

-- | There is another handy function available that you may wish to
--   consider; specialised, it becomes:
--
--   > mapM_ :: (a -> Parser ()) -> [a] -> Parser ()
--
--   This is similar to 'map', but only cares about /performing/ the
--   actions, not the results of them.  It has the effect of running
--   every specified parser in turn.
concatenatedAtomsToParser :: ConcatenatedAtoms -> Parser ()
concatenatedAtomsToParser (ConcatenatedAtoms qas) = mapM_ quantifiedAtomToParser qas

-- | You may wish to take use these functions (all from
--   "Control.Applicative"):
--
--   > optional :: Parser a -> Parser (Maybe a)
--
--   > some :: Parser a -> Parser [a]
--
--   > many :: Parser a -> Parser [a]
quantifiedAtomToParser :: QuantifiedAtom -> Parser ()
quantifiedAtomToParser (PlainAtom at)    =                 atomToParser at
quantifiedAtomToParser (OptionalAtom at) = void (optional (atomToParser at))
quantifiedAtomToParser (AtLeastOne at)   = void (some     (atomToParser at))
quantifiedAtomToParser (Multiple at)     = void (many     (atomToParser at))

atomToParser :: Atom -> Parser ()
atomToParser AnyChar          = void next
atomToParser (SpecificChar c) = void (char c)
atomToParser (BExpression be) = bracketExpressionToParser be
atomToParser (SubPattern pat) = regexToParser pat

bracketExpressionToParser :: BracketExpression -> Parser ()
bracketExpressionToParser = void . satisfy . satisfiesBracketExpression

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
satisfiesBracketExpression (BracketExpression inv patterns) =
  \c -> includeOrExclude (any (satisfiesBracketPattern c) patterns)
  where
    includeOrExclude = if inv
                          then not
                          else id

{-# ANN satisfiesBracketExpression ("HLint: ignore Redundant bracket" :: String) #-}

-- | Check if a character is matched by the 'BracketPattern'.
satisfiesBracketPattern :: Char -> BracketPattern -> Bool
satisfiesBracketPattern c bp = case bp of
                                 BracketRange l u -> l <= c && c <= u
                                 BracketChar  p   -> c == p
