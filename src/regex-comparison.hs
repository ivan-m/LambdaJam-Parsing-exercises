{- |
   Module      : Main
   Description : Compare performance of regex parsers
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com



 -}
module Main (main) where

import Regex.Types

import qualified Regex.CommitParser    as CommitParser
import qualified Regex.CPSCommitParser as CPSCommitParser
import qualified Regex.CPSParser       as CPSParser
import qualified Regex.SimpleParser    as SimpleParser

import TestBench

--------------------------------------------------------------------------------

-- Uncomment the appropriate lines in this section once you've
-- implemented each parser.
parserList :: [Parsers]
parserList = [ SimpleParser
             -- , CommitParser
             -- , CPSParser
             -- , CPSCommitParser
             ]

data Parsers = SimpleParser
             | CommitParser
             | CPSParser
             | CPSCommitParser
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parseRegex :: Parsers -> String -> Maybe Pattern
parseRegex SimpleParser    = SimpleParser.parseRegex
parseRegex CommitParser    = CommitParser.parseRegex
parseRegex CPSParser       = CPSParser.parseRegex
parseRegex CPSCommitParser = CPSParser.parseRegex

applyRegex :: Parsers -> String -> String -> Bool
applyRegex SimpleParser    = SimpleParser.applyRegex
applyRegex CommitParser    = CommitParser.applyRegex
applyRegex CPSParser       = CPSParser.applyRegex
applyRegex CPSCommitParser = CPSParser.applyRegex

--------------------------------------------------------------------------------

main :: IO ()
main = testBench $ do
  parseBench "Deep nesting"
             (replicate long '(' <> "[ab]" <> replicate long ')')

  parseBench "Deep nesting (unbalanced inner)"
             (replicate long '(' <> "[ab" <> replicate long ')')

  parseBench "Deep nesting (unbalanced outer)"
             (replicate long '(' <> "[ab]" <> replicate (long-1) ')')

  applyBench "a*b|c" [ ("Case 1, short", "b")
                     , ("Case 1, long", longAs <> "b")
                     , ("Case 2", "c")
                     , ("Failure, short", "d")
                     , ("Failure, long (a*)", longAs)
                     , ("Failure, long (a*d)", longAs <> "d")
                     ]

-- | Take a description, then try to parse a regex for every 'Parsers'
--   value.
parseBench :: String -> String -> TestBench
parseBench desc re = compareFuncList desc (`parseRegex` re) normalForm parserList

-- | Takes a regex and a list of @(description, test string)@ pairs.
--   Creates a benchmark for every pair for every 'Parsers' value.
applyBench :: String -> [(String, String)] -> TestBench
applyBench re = collection re . mapM_ mkCase
  where
    mkCase (desc,str) = compareFuncList desc (\p -> applyRegex p re str) normalForm parserList

longAs :: String
longAs = replicate long 'a'

long :: Int
long = 100000
