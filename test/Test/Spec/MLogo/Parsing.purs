module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (BracketType(..), Token(..))
import MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
  , NumericLiteral(..)
  , Parameter(..)
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
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement
                      $ NumericLiteralExpression
                      $ NumberLiteral 1.0
                  ]
              )
          ]
      )

    testCase
      "procedure call with a numeric literal - question mark suffix"
      [ UnquotedWord "proc1?"
      , NumberToken 1.0
      ]
      ( Right $
          [ ProcedureCall
              "proc1?"
              ( List.fromFoldable
                  [ ExpressionStatement
                      $ NumericLiteralExpression
                      $ NumberLiteral 1.0
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
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1"
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
      , LineBreak
      , UnquotedWord "proc2"
      , ColonPrefixedWord "param1"
      , LineBreak
      , UnquotedWord "proc3"
      , ColonPrefixedWord "param2"
      , LineBreak
      , UnquotedWord "output"
      , NumberToken 1.0
      , LineBreak
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
                  [ ProcedureCall
                      "proc2"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ VariableReference "param1"
                          ]
                      )
                  , ProcedureCall
                      "proc3"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ VariableReference "param2"
                          ]
                      )
                  , ControlStructureStatement
                      $ OutputCall
                      $ ExpressionStatement
                      $ NumericLiteralExpression
                      $ NumberLiteral 1.0
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
      , LineBreak
      , UnquotedWord "proc2"
      , NumberToken 2.0
      , Bracket SquareClosing
      ]
      ( Right $
          [ ControlStructureStatement $ IfBlock
              ( ProcedureCall "equal?"
                  ( List.fromFoldable
                      [ ExpressionStatement $ VariableReference
                          "var1"
                      , ExpressionStatement $ VariableReference
                          "var2"
                      ]
                  )
              )
              ( List.fromFoldable
                  [ ProcedureCall
                      "proc1"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ NumericLiteralExpression
                              $ NumberLiteral 1.0
                          ]
                      )
                  , ProcedureCall
                      "proc2"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ NumericLiteralExpression
                              $ NumberLiteral 2.0
                          ]
                      )
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls - separate lines"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      , LineBreak
      , UnquotedWord "proc2"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , LineBreak
      , UnquotedWord "proc3"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , ColonPrefixedWord "var3"
      ]
      ( Right $
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1" ]
              )
          , ProcedureCall
              "proc2"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference
                      "var1"
                  , ExpressionStatement $ VariableReference
                      "var2"
                  ]
              )
          , ProcedureCall
              "proc3"
              ( List.fromFoldable
                  [ ExpressionStatement $
                      VariableReference "var1"
                  , ExpressionStatement $
                      VariableReference "var2"
                  , ExpressionStatement $
                      VariableReference "var3"
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls - separate lines, redundant line breaks"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      , LineBreak
      , LineBreak
      , UnquotedWord "proc2"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , LineBreak
      , LineBreak
      , UnquotedWord "proc3"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , ColonPrefixedWord "var3"
      ]
      ( Right $
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1" ]
              )
          , ProcedureCall
              "proc2"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference
                      "var1"
                  , ExpressionStatement $ VariableReference
                      "var2"
                  ]
              )
          , ProcedureCall
              "proc3"
              ( List.fromFoldable
                  [ ExpressionStatement $
                      VariableReference "var1"
                  , ExpressionStatement $
                      VariableReference "var2"
                  , ExpressionStatement $
                      VariableReference "var3"
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls - same line"
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
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1" ]
              )
          , ProcedureCall
              "proc2"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference
                      "var1"
                  , ExpressionStatement $ VariableReference
                      "var2"
                  ]
              )
          , ProcedureCall
              "proc3"
              ( List.fromFoldable
                  [ ExpressionStatement $
                      VariableReference "var1"
                  , ExpressionStatement $
                      VariableReference "var2"
                  , ExpressionStatement $
                      VariableReference "var3"
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls - same line, order determined by parenthesis"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      , Bracket RoundOpening
      , UnquotedWord "proc2"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , Bracket RoundClosing
      , Bracket RoundOpening
      , UnquotedWord "proc3"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , ColonPrefixedWord "var3"
      , Bracket RoundClosing
      ]
      ( Right $
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1"
                  , ProcedureCall
                      "proc2"
                      ( List.fromFoldable
                          [ ExpressionStatement $ VariableReference
                              "var1"
                          , ExpressionStatement $ VariableReference
                              "var2"
                          ]
                      )
                  , ProcedureCall
                      "proc3"
                      ( List.fromFoldable
                          [ ExpressionStatement $
                              VariableReference "var1"
                          , ExpressionStatement $
                              VariableReference "var2"
                          , ExpressionStatement $
                              VariableReference "var3"
                          ]
                      )
                  ]
              )
          ]
      )

    testCase
      "multiple procedure calls - mixed arrangement"
      [ UnquotedWord "proc1"
      , ColonPrefixedWord "var1"
      , UnquotedWord "proc2"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , LineBreak
      , UnquotedWord "proc3"
      , ColonPrefixedWord "var1"
      , ColonPrefixedWord "var2"
      , ColonPrefixedWord "var3"
      ]
      ( Right $
          [ ProcedureCall
              "proc1"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference "var1" ]
              )
          , ProcedureCall
              "proc2"
              ( List.fromFoldable
                  [ ExpressionStatement $ VariableReference
                      "var1"
                  , ExpressionStatement $ VariableReference
                      "var2"
                  ]
              )
          , ProcedureCall
              "proc3"
              ( List.fromFoldable
                  [ ExpressionStatement $
                      VariableReference "var1"
                  , ExpressionStatement $
                      VariableReference "var2"
                  , ExpressionStatement $
                      VariableReference "var3"
                  ]
              )
          ]
      )

testCase
  ∷ String → Array Token → ParseError \/ Array Statement → Spec Unit
testCase title tokens expected = it
  ("parses \"" <> title <> "\"")
  ( (Parsing.run $ List.fromFoldable tokens)
      `shouldEqual`
        (List.fromFoldable <$> expected)
  )

