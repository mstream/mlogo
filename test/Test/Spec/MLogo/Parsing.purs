module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (Token(..))
import MLogo.Parsing (Expression(..), ProcedureCall(..), Statement(..))
import MLogo.Parsing as Parsing
import Parsing (ParseError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "run" do

    testCase
      "example1"
      [ UnquotedWord "proc1"
      , Number 1
      , QuotedWord "arg1"
      ]
      ( Right $
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ NumericLiteral 1
                  , WordLiteral "arg1"
                  ]
              )
          ]
      )

testCase :: String -> Array Token -> ParseError \/ Array Statement -> Spec Unit
testCase title tokens expected = it
  ("parses \"" <> title <> "\"")
  ((Parsing.run $ List.fromFoldable tokens) `shouldEqual` (List.fromFoldable <$> expected))

