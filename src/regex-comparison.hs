{- |
   Module      : Main
   Description : Compare performance of regex parsers
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com



 -}
module Main (main) where

import Regex.Types

import qualified Regex.SimpleParser as SimpleParser

import TestBench

--------------------------------------------------------------------------------

main :: IO ()
main = testBench $ do
  parseBench "Deep nesting"
             (replicate 10000 '(' ++ "[ab]" ++ replicate 10000 ')')

  parseBench "Deep nesting (unbalanced inner)"
             (replicate 10000 '(' ++ "[ab" ++ replicate 10000 ')')

  parseBench "Deep nesting (unbalanced outer)"
             (replicate 10000 '(' ++ "[ab]" ++ replicate 9999 ')')

  applyBench "a*b|c" [ ("Case 1, short", "b")
                     , ("Case 1, long", longAs ++ "b")
                     , ("Case 2", "c")
                     , ("Failure, short", "d")
                     , ("Failure, long (a*)", longAs)
                     , ("Failure, long (a*d)", longAs ++ "d")
                     ]

-- | Take a description, then try to parse a regex for every 'Parsers'
--   value.
parseBench :: String -> String -> TestBench
parseBench desc re = compareFuncAll desc (`parseRegex` re) normalForm

-- | Takes a regex and a list of @(description, test string)@ pairs.
--   Creates a benchmark for every pair for every 'Parsers' value.
applyBench :: String -> [(String, String)] -> TestBench
applyBench re = collection re . mapM_ mkCase
  where
    mkCase (desc,str) = compareFuncAll desc (\p -> applyRegex p re str) normalForm

longAs :: String
longAs = replicate 10000 'a'

--------------------------------------------------------------------------------

data Parsers = SimpleParser
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parseRegex :: Parsers -> String -> Maybe Pattern
parseRegex SimpleParser = SimpleParser.parseRegex

applyRegex :: Parsers -> String -> String -> Bool
applyRegex SimpleParser = SimpleParser.applyRegex
