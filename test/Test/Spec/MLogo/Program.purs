module Test.Spec.MLogo.Program (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.String as String
import MLogo.Interpretation.State (Position(..), VisibleState)
import MLogo.Program as Program
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Program" do
  describe "run" do

    testCase
      "empty program"
      ""
      ( Right $
          { pointer:
              { angle: zero
              , isDown: true
              , position: zero
              }
          , screen: Nil
          }
      )

    testCase
      "moving forward by 10"
      ( String.joinWith
          "\n"
          [ "forward 10" ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , isDown: true
              , position:
                  Position
                    { x: 0.0
                    , y: 10.0
                    }
              }
          , screen: List.fromFoldable
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          }
      )

  testCase
    "moving forward by 10 using a custom procedure"
    ( String.joinWith
        "\n"
        [ "to go :steps"
        , "forward :steps"
        , "end"
        , "go 10"
        ]
    )
    ( Right $
        { pointer:
            { angle: zero
            , isDown: true
            , position:
                Position
                  { x: 0.0
                  , y: 10.0
                  }
            }
        , screen: List.fromFoldable
            [ { p1: Position { x: 0.0, y: 0.0 }
              , p2: Position { x: 0.0, y: 10.0 }
              }
            ]
        }
    )

  testCase
    "moving forward by 10 using variable assignments and conditionals"
    ( String.joinWith
        "\n"
        [ "make \"steps 5"
        , "make \"t true"
        , "make \"f false"
        , "if :f [ back :steps ]"
        , "if :t [ forward :steps ]"
        , "ifelse :t [ forward :steps ] [ back :steps ]"
        ]
    )
    ( Right $
        { pointer:
            { angle: zero
            , isDown: true
            , position:
                Position
                  { x: 0.0
                  , y: 10.0
                  }
            }
        , screen: List.fromFoldable
            [ { p1: Position { x: 0.0, y: 5.0 }
              , p2: Position { x: 0.0, y: 10.0 }
              }
            , { p1: Position { x: 0.0, y: 0.0 }
              , p2: Position { x: 0.0, y: 5.0 }
              }
            ]
        }
    )

  testCase
    "moving forward by 10 using the repeat statement"
    ( String.joinWith
        "\n"
        [ "repeat 2 [ forward 5 ]"
        ]
    )
    ( Right $
        { pointer:
            { angle: zero
            , isDown: true
            , position:
                Position
                  { x: 0.0
                  , y: 10.0
                  }
            }
        , screen: List.fromFoldable
            [ { p1: Position { x: 0.0, y: 5.0 }
              , p2: Position { x: 0.0, y: 10.0 }
              }
            , { p1: Position { x: 0.0, y: 0.0 }
              , p2: Position { x: 0.0, y: 5.0 }
              }
            ]
        }
    )

testCase
  ∷ String → String → String \/ VisibleState → Spec Unit
testCase title source expected = it
  ("executes \"" <> title <> "\"")
  ((Program.run source) `shouldEqual` expected)

