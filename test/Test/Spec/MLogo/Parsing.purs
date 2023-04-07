module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (BracketType(..), Token(..))
import MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
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
      , NumberToken 1.0
      ]
      ( Right $
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ NumericLiteral 1.0
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
      "if block"
      [ UnquotedWord "if"
      , Bracket RoundOpening
      , UnquotedWord "equal?"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , Bracket RoundClosing
      , Bracket SquareOpening
      , UnquotedWord "proc1"
      , NumberToken 1.0
      , UnquotedWord "proc2"
      , NumberToken 2.0
      , Bracket SquareClosing
      ]
      ( Right $
          [ ControlStructureStatement $ IfBlock
              ( ProcedureCallExpression $ ProcedureCall "equal?"
                  ( List.fromFoldable
                      [ VariableReference "var1"
                      , VariableReference "var2"
                      ]
                  )
              )
              ( List.fromFoldable
                  [ ProcedureCallStatement $ ProcedureCall
                      "proc1"
                      (List.fromFoldable [ NumericLiteral 1.0 ])
                  , ProcedureCallStatement $ ProcedureCall
                      "proc2"
                      (List.fromFoldable [ NumericLiteral 2.0 ])
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      , UnquotedWord "proc2"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , UnquotedWord "proc3"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , ColonPrefixedWord "var3"
      ]
      ( Right $
          [ ProcedureCallStatement $ ProcedureCall
              "proc1"
              (List.fromFoldable [ VariableReference "var1" ])
          , ProcedureCallStatement $ ProcedureCall
              "proc2"
              ( List.fromFoldable
                  [ VariableReference "var1"
                  , VariableReference "var2"
                  ]
              )
          , ProcedureCallStatement $ ProcedureCall
              "proc3"
              ( List.fromFoldable
                  [ VariableReference "var1"
                  , VariableReference "var2"
                  , VariableReference "var3"
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

