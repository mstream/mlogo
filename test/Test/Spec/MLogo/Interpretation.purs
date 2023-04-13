module Test.Spec.MLogo.Interpretation (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.State
  ( ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
  , NumericLiteral(..)
  , Parameter(..)
  , Statement(..)
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Interpretation" do
  describe "run" do
    testCase
      "calling a command directly using a literal"
      [ ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement
                  $ NumericLiteralExpression
                  $ NumberLiteral 10.0
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.empty
          }
      )

    testCase
      "calling a command multiple times using a repeat statement"
      [ ControlStructureStatement $ RepeatBlock
          ( ExpressionStatement $ NumericLiteralExpression $
              IntegerLiteral 2
          )
          ( List.fromFoldable
              [ ProcedureCall
                  "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 20.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 10.0 }
                , p2: Position { x: 0.0, y: 20.0 }
                }
              , { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.empty
          }
      )

    testCase
      "moving home"
      [ ProcedureCall
          "penup"
          Nil
      , ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      , ProcedureCall
          "home"
          Nil
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: false
              , position: zero
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.empty
          }
      )

    testCase
      "moving cursor with a pen up"
      [ ProcedureCall
          "penup"
          Nil
      , ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: false
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.empty
          }
      )

    testCase
      "cleaning after drawing"
      [ ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      , ProcedureCall
          "clean"
          Nil
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.empty
          }
      )

    testCase
      "clearing screen after drawing"
      [ ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      , ProcedureCall
          "clearscreen"
          Nil
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position: zero
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.empty
          }
      )

    testCase
      "calling a command directly using a variable reference"
      [ ProcedureCall
          "make"
          ( List.fromFoldable
              [ ExpressionStatement $ WordLiteral "steps"
              , ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      , ProcedureCall
          "forward"
          ( List.fromFoldable
              [ ExpressionStatement $ NumericLiteralExpression $
                  NumberLiteral 10.0
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.fromFoldable
              [ "steps" /\ NumberValue 10.0
              ]
          }
      )

    ( let
        procedureName1 = "procedure1"
        procedureParameters1 =
          (List.fromFoldable [ Parameter "parameter1" ])
        procedureBody1 =
          ( List.fromFoldable
              [ ProcedureCall "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ VariableReference
                          "parameter1"
                      ]
                  )
              ]
          )
      in
        testCase
          "calling a command through a procedure using a parameter reference"
          [ ProcedureDefinition
              procedureName1
              procedureParameters1
              procedureBody1
          , ProcedureCall
              procedureName1
              ( List.fromFoldable
                  [ ExpressionStatement $ NumericLiteralExpression $
                      NumberLiteral 10.0
                  ]
              )
          ]
          ( Right $ Newtype.wrap
              { callStack: Nil
              , outputtedValue: Nothing
              , pointer:
                  { angle: zero
                  , isDown: true
                  , position:
                      Position
                        { x: 0.0
                        , y: 10.0
                        }
                  }
              , procedures: Map.fromFoldable
                  [ procedureName1 /\
                      { body: procedureBody1
                      , parameters: procedureParameters1
                      }
                  ]
              , screen: List.fromFoldable
                  [ { p1: Position { x: 0.0, y: 0.0 }
                    , p2: Position { x: 0.0, y: 10.0 }
                    }
                  ]
              , variables: Map.empty
              }
          )
    )

    testCase
      "running a command conditionally"
      [ ControlStructureStatement $ IfBlock
          (ExpressionStatement $ BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall
                  "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.empty
          }
      )

    testCase
      "not running a command conditionally"
      [ ControlStructureStatement $ IfBlock
          (ExpressionStatement $ BooleanLiteral false)
          ( List.fromFoldable
              [ ProcedureCall
                  "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position: zero
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.empty
          }
      )

    testCase
      "running a command conditionally where condition is a variable reference"
      [ ProcedureCall
          "make"
          ( List.fromFoldable
              [ ExpressionStatement $ WordLiteral "condition"
              , ExpressionStatement $ BooleanLiteral true
              ]
          )
      , ControlStructureStatement
          $ IfBlock
              (ExpressionStatement $ VariableReference "condition")
              ( List.fromFoldable
                  [ ProcedureCall
                      "forward"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ NumericLiteralExpression
                              $ NumberLiteral 10.0
                          ]
                      )
                  ]
              )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.fromFoldable
              [ "condition" /\ BooleanValue true
              ]
          }
      )

    testCase
      "not running a command conditionally where condition is a variable reference"
      [ ProcedureCall
          "make"
          ( List.fromFoldable
              [ ExpressionStatement $ WordLiteral "condition"
              , ExpressionStatement $ BooleanLiteral false
              ]
          )
      , ControlStructureStatement
          $ IfBlock
              (ExpressionStatement $ VariableReference "condition")
              ( List.fromFoldable
                  [ ProcedureCall
                      "forward"
                      ( List.fromFoldable
                          [ ExpressionStatement
                              $ NumericLiteralExpression
                              $ NumberLiteral 10.0
                          ]
                      )
                  ]
              )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position: zero
              }
          , procedures: Map.empty
          , screen: Nil
          , variables: Map.fromFoldable
              [ "condition" /\ BooleanValue false
              ]
          }
      )

    testCase
      "running the positive branch of an if statement"
      [ ControlStructureStatement $ IfElseBlock
          (ExpressionStatement $ BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall
                  "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
          ( List.fromFoldable
              [ ProcedureCall
                  "back"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , variables: Map.empty
          }
      )

    testCase
      "running the negative branch of an if statement"
      [ ControlStructureStatement $ IfElseBlock
          (ExpressionStatement $ BooleanLiteral false)
          ( List.fromFoldable
              [ ProcedureCall
                  "forward"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
          ( List.fromFoldable
              [ ProcedureCall
                  "back"
                  ( List.fromFoldable
                      [ ExpressionStatement $ NumericLiteralExpression $
                          NumberLiteral 10.0
                      ]
                  )
              ]
          )
      ]
      ( Right $ Newtype.wrap
          { callStack: Nil
          , outputtedValue: Nothing
          , pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: -10.0
                    }
              }
          , procedures: Map.empty
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: -10.0 }
                }
              ]
          , variables: Map.empty
          }
      )

    ( let
        procedureName1 = "procedure1"
        procedureParameters1 = Nil
        procedureBody1 =
          ( List.fromFoldable
              [ ControlStructureStatement
                  $ OutputCall
                  $ ExpressionStatement
                  $ NumericLiteralExpression
                  $ NumberLiteral 10.0
              ]
          )
      in
        testCase
          "outputting a value from a procedure"
          [ ProcedureDefinition
              procedureName1
              procedureParameters1
              procedureBody1
          , ProcedureCall
              "forward"
              ( List.fromFoldable
                  [ ProcedureCall
                      procedureName1
                      Nil
                  ]
              )
          ]
          ( Right $ Newtype.wrap
              { callStack: Nil
              , outputtedValue: Nothing
              , pointer:
                  { angle: zero
                  , isDown: true
                  , position:
                      Position
                        { x: 0.0
                        , y: 10.0
                        }
                  }
              , procedures: Map.fromFoldable
                  [ procedureName1 /\
                      { body: procedureBody1
                      , parameters: procedureParameters1
                      }
                  ]
              , screen: List.fromFoldable
                  [ { p1: Position { x: 0.0, y: 0.0 }
                    , p2: Position { x: 0.0, y: 10.0 }
                    }
                  ]
              , variables: Map.empty
              }
          )
    )

testCase
  ∷ String → Array Statement → String \/ ExecutionState → Spec Unit
testCase title statements expected = it
  ("interprets \"" <> title <> "\"")
  ( (Interpretation.run $ List.fromFoldable statements) `shouldEqual`
      expected
  )

