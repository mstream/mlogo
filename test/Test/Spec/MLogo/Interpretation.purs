module Test.Spec.MLogo.Interpretation (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
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
  , Parameter(..)
  , ProcedureCall(..)
  , Statement(..)
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Interpretation" do
  describe "run" do

    testCase
      "calling a command directly using a literal"
      [ ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      ]
      ( Right $
          { callStack: Nil
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
      "moving home"
      [ ProcedureCallStatement $ ProcedureCall
          "penup"
          Nil
      , ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      , ProcedureCallStatement $ ProcedureCall
          "home"
          Nil
      ]
      ( Right $
          { callStack: Nil
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
      [ ProcedureCallStatement $ ProcedureCall
          "penup"
          Nil
      , ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      ]
      ( Right $
          { callStack: Nil
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
      [ ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      , ProcedureCallStatement $ ProcedureCall
          "clean"
          Nil
      ]
      ( Right $
          { callStack: Nil
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
      [ ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      , ProcedureCallStatement $ ProcedureCall
          "clearscreen"
          Nil
      ]
      ( Right $
          { callStack: Nil
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
      [ ProcedureCallStatement $ ProcedureCall
          "make"
          ( List.fromFoldable
              [ WordLiteral "steps"
              , NumericLiteral 10
              ]
          )
      , ProcedureCallStatement $ ProcedureCall
          "forward"
          (List.fromFoldable [ NumericLiteral 10 ])
      ]
      ( Right $
          { callStack: Nil
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
              [ "steps" /\ NumberValue 10
              ]
          }
      )

    ( let
        procedureName1 = "procedure1"
        procedureParameters1 =
          (List.fromFoldable [ Parameter "parameter1" ])
        procedureBody1 =
          ( List.fromFoldable
              [ ProcedureCallStatement $ ProcedureCall "forward"
                  ( List.fromFoldable
                      [ VariableReference "parameter1"
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
          , ProcedureCallStatement $ ProcedureCall
              procedureName1
              (List.fromFoldable [ NumericLiteral 10 ])
          ]
          ( Right $
              { callStack: Nil
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
        (BooleanLiteral true)
        ( List.fromFoldable
            [ ProcedureCallStatement $ ProcedureCall
                "forward"
                (List.fromFoldable [ NumericLiteral 10 ])
            ]
        )
    ]
    ( Right $
        { callStack: Nil
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
        (BooleanLiteral false)
        ( List.fromFoldable
            [ ProcedureCallStatement $ ProcedureCall
                "forward"
                (List.fromFoldable [ NumericLiteral 10 ])
            ]
        )
    ]
    ( Right $
        { callStack: Nil
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
  ∷ String → Array Statement → String \/ ExecutionState → Spec Unit
testCase title statements expected = it
  ("interprets \"" <> title <> "\"")
  ( (Interpretation.run $ List.fromFoldable statements) `shouldEqual`
      expected
  )

