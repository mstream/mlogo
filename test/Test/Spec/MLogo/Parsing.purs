module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (Token(..))
import MLogo.Parsing
  ( Expression(..)
  , Parameter(..)
  , ProcedureCall(..)
  , Statement(..)
  )
import MLogo.Parsing as Parsing
import Parsing (ParseError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "run" do

    testCase
      "procedure call with a numeric literal"
      [ UnquotedWord "proc1"
      , Number 1
      ]
      ( Right $
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ NumericLiteral 1
                  ]
              )
          ]
      )

    testCase
      "procedure call with a colon-prefixed word"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      ]
      ( Right $
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ VariableReference "var1"
                  ]
              )
          ]
      )

    testCase
      "procedure definition"
      [ UnquotedWord "to"
      , UnquotedWord "proc1"
      , ColonPrefixedWord "param1"
      , ColonPrefixedWord "param2"
      , UnquotedWord "proc2"
      , ColonPrefixedWord "param1"
      , UnquotedWord "proc3"
      , ColonPrefixedWord "param2"
      , UnquotedWord "end"
      ]
      ( Right $
          [ ProcedureDefinition
              "proc1"
              ( List.fromFoldable
                  [ Parameter "param1"
                  , Parameter "param2"
                  ]
              )
              ( List.fromFoldable
                  [ ProcedureCallStatement $ ProcedureCall
                      "proc2"
                      ( List.fromFoldable
                          [ VariableReference "param1"
                          ]
                      )
                  , ProcedureCallStatement $ ProcedureCall
                      "proc3"
                      ( List.fromFoldable
                          [ VariableReference "param2"
                          ]
                      )
                  ]
              )
          ]
      )

testCase
  ∷ String → Array Token → ParseError \/ Array Statement → Spec Unit
testCase title tokens expected = it
  ("parses \"" <> title <> "\"")
  ( (Parsing.run $ List.fromFoldable tokens) `shouldEqual`
      (List.fromFoldable <$> expected)
  )

