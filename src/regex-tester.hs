{- |
   Module      : Main
   Description : Test regex parsing implementation
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : ivan.miljenovic@gmail.com

  This provides a test suite for your regex parser implementations.

 -}
module Main where

import Test.Hspec

import Regex.Types

import Regex.SimpleParser

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "parseRegex" $ do
    it "Parses empty regex" $
      parseRegex "" `shouldBe` Just (Pattern [])

    it "Parses ." $
      parseRegex "." `shouldBe` singleAtomM AnyChar

    it "Parses escaped characters" $
      parseRegex "\\." `shouldBe` singleAtomM (SpecificChar '.')

    it "Parses bracket expression" $
      parseRegex "[1a-zA-Z9]" `shouldBe` singleAtomM (BExpression $ BracketExpression False sampleBracketPatterns)

    it "Parses inverse bracket expression" $
      parseRegex "[^1a-zA-Z9]" `shouldBe` singleAtomM (BExpression $ BracketExpression True sampleBracketPatterns)

    it "Parses nested patterns" $
      parseRegex "((.))" `shouldBe` singleAtomM (SubPattern (singleAtom (SubPattern (singleAtom AnyChar))))

    it "Parses complicated regex" $
      parseRegex "abc|(de)+f|ge?|[x-z]" `shouldBe` Just (mkPattern [ map plainChar ['a', 'b', 'c']
                                                                   , [AtLeastOne (SubPattern (mkPattern [[plainChar 'd', plainChar 'e']])), plainChar 'f']
                                                                   , [plainChar 'g', OptionalAtom (SpecificChar 'e')]
                                                                   , [PlainAtom . BExpression $ BracketExpression False [BracketRange 'x' 'z']]
                                                                   ])

    describe "Doesn't parse invalid expressions" $ do
      describe "Bracket Expressions" $ do
        it "Non-alphanum pattern" $
          parseRegex "[{]" `shouldBe` Nothing
        it "Non-closed range" $
          parseRegex "[1a-]" `shouldBe` Nothing
        it "Missing closing bracket" $
          parseRegex "[a-z" `shouldBe` Nothing
        it "Missing opening bracket" $
          parseRegex "a-z]" `shouldBe` Nothing
        it "No patterns" $
          parseRegex "[]" `shouldBe` Nothing

      it "Un-escaped meta character" $
        parseRegex "?" `shouldBe` Nothing

      it "Un-closed sub-pattern" $
        parseRegex "(a" `shouldBe` Nothing

      it "Multiple quantifiers" $
        parseRegex "a+*" `shouldBe` Nothing

      it "Double alternation operator" $
        parseRegex "a||b" `shouldBe` Nothing

      it "Trailing alternation operator" $
        parseRegex "a|b|" `shouldBe` Nothing

  describe "applyRegex" $ do
    describe "Empty regex" $ do
      let test = applyRegex ""
      it "Matches empty string" $
        "" `shouldSatisfy` test
      it "Doesn't match non-empty string" $
        "a" `shouldNotSatisfy` test

    {-

    This test fails; can you tell why? (Hint: what does "." match?)
    That said, this regex is overly simplistic even compared to other
    implementations: https://emailregex.com/

    describe "Simple email test" $ do
      let test = applyRegex ".+@.+"
      it "Matches email" $
        "test@example.com" `shouldSatisfy` test
      it "Doesn't match non-email" $
        "noDomain@" `shouldNotSatisfy` test
    -}

    describe "Manual digit test" $ do
      let test = applyRegex "1|2|3|4|5|6|7|8|9|0"
      it "Matches digit" $
        "3" `shouldSatisfy` test
      it "Doesn't match multiple digits" $
        "23" `shouldNotSatisfy` test
      it "Doesn't match letter" $
        "a" `shouldNotSatisfy` test

    describe "a*b" $ do
      let test = applyRegex "a*b"
          as   = replicate 100 'a'
      it "Succeeds" $
        (as ++ "b") `shouldSatisfy` test
      it "Fails" $
        as `shouldNotSatisfy` test

singleAtomM :: Atom -> Maybe Pattern
singleAtomM = Just . singleAtom

singleAtom :: Atom -> Pattern
singleAtom at = mkPattern [[PlainAtom at]]

mkPattern :: [[QuantifiedAtom]] -> Pattern
mkPattern = Pattern . map ConcatenatedAtoms

sampleBracketPatterns :: [BracketPattern]
sampleBracketPatterns = [ BracketChar '1'
                        , BracketRange 'a' 'z'
                        , BracketRange 'A' 'Z'
                        , BracketChar '9'
                        ]

plainChar :: Char -> QuantifiedAtom
plainChar = PlainAtom . SpecificChar
