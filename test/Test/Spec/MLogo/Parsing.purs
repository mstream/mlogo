module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import MLogo.Lexing (Token(..))
import MLogo.Parsing (Expression(..), ProcedureCall(..), Statement(..))
import MLogo.Parsing as Parsing
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "run" do
    it "parses example1" do
      let
        tokens = List.fromFoldable
          [ UnquotedWord "proc1"
          , Number 1
          , QuotedWord "arg1"
          ]
        actual = Parsing.run tokens
        expected = Right $ List.fromFoldable
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ NumericLiteral 1
                  , WordLiteral "arg1"
                  ]
              )
          ]

      actual `shouldEqual` expected

