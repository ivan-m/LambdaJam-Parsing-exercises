{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

{- |
   Module      : Regex.Types
   Description : Regex types used for parsing
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

  This module defines data types corresponding to a subset of POSIX
  Extended Regular Expressions, defined in the 'Pattern' type.  Not
  covered are:

  * Begin\/end markers @^@ and @$@

  * Sub-expression matching (@\1@, @\2@, etc.)

  * Matching an atom a specified number of times (e.g. @a{2,5}@ will
    match between 2 and 5 @a@s).

  * Character classes for use with Bracket Expressions.

 -}
module Regex.Types where

import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

--------------------------------------------------------------------------------

-- | At the top level, a Regex Pattern is a set of strings separated
--   by the choice\/alternation operator \"@|@\".  For example:
--
--   @
--     abc|(de)+f|ge?|[x-z]
--
--   @
--
--   (That is, any of the sub-patterns are allowed.)
newtype Pattern = Pattern [ConcatenatedAtoms] -- Separated by @|@
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | A group of individual atoms concatenated together.  For example,
--   @ab@ is the concatenation of the patterns @a@ and @b@.
--
--   Should be non-empty.
newtype ConcatenatedAtoms = ConcatenatedAtoms [QuantifiedAtom]
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | Atoms possibly tagged with a matching operator.  For example:
--
--   * @a@ just matches the character @a@
--
--   * @a?@ matches @a@ exactly 0 or 1 times
--
--   * @a+@ matches @a@ at /least/ once
--
--   * @a*@ matches @a@ 0 or more times
--
--   This isn't necessarily the best implementation of this data
--   structure, but is instead aimed at helping making the focus on
--   how to write a parser.
data QuantifiedAtom = PlainAtom    Atom
                    | OptionalAtom Atom -- ^ @?@
                    | AtLeastOne   Atom -- ^ @+@
                    | Multiple     Atom -- ^ @*@
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | A specific unique element.
--
--   * @.@ is a wildcard and matches any character.
--
--   * A single character not in 'metaChars' matches that character
--     precisely.  A character in 'metaChars' escaped with @\\@ can
--     also be matched.
--
--   * A 'BracketExpression' @[a-z]@.
--
--   * A 'Pattern' in parentheses @([a-z]|3+)@
data Atom = AnyChar
          | SpecificChar Char
          | BExpression BracketExpression
          | SubPattern Pattern
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | Match a single character defined by a 'BracketPattern' (or if it
--   starts with @^@ then match any single character /not/ defined by
--   a 'BracketPattern').  For example:
--
--   * @[1-9]@ will match a digit.
--
--   * @[^a-zA-Z]@ will match any /non/ (ASCII) alphabetical
--     character.
--
--   No escaping required\/allowed for characters in 'metaChars'.
data BracketExpression = BracketExpression
  { -- | Starts with @^@
    inverseExpression :: Bool

    -- | Non-empty list.
  , bracketPatterns   :: [BracketPattern]
  } deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | A pattern in a 'BracketExpression' can either be a range such as
--   @a-z@ or a single character (no escaping required\/allowed).
--
--   To simplify, we only consider alphanumeric characters (otherwise
--   we need to handle many special cases for how to handle specific
--   symbols.  See how complicated regexes are?).
data BracketPattern = BracketRange Char Char
                    | BracketChar  Char
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | The characters that must typically be escaped.
metaChars :: [Char]
metaChars = ['|', '\\', '(', ')', '[', ']', '*', '+', '?', '.']

{-# ANN metaChars ("HLint: ignore Use String" :: String) #-}
{-# ANN metaChars ("HLint: ignore Use string literal" :: String) #-}
