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
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Utils as Utils

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
          , "if (:f) [ back :steps ]"
          , "if (:t) [ forward :steps ]"
          , "ifelse (:t) [ forward :steps ] [ back :steps ]"
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
      "moving forward by 10 using a conditional with a predicate"
      ( String.joinWith
          "\n"
          [ "if (1 = 1) [ fd 10 ]" ]
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
      "not moving forward using a conditional with a predicate"
      ( String.joinWith
          "\n"
          [ "if (1 = 2) [ fd 10 ]" ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , isDown: true
              , position: zero
              }
          , screen: Nil
          }
      )

  compatibilityTestCase
    "Dahlia, by David Eisenstat, U.S. (14 words)"
    "repeat 8 [rt 45 repeat 6 [repeat 90 [fd 2 rt 2] rt 90]]"

  compatibilityTestCase
    "Hairy Star"
    "for [i 0 4700] [fd 10 rt (180 * sin (:i * :i))]"

testCase
  ∷ String → String → String \/ VisibleState → Spec Unit
testCase title source expected = it ("executes \"" <> title <> "\"") do
  let
    actual = Program.run source
  if actual == expected then pure unit
  else fail $
    "--- error >>> ---\n"
      <> show actual
      <> "\nis not equal to\n"
      <> show expected
      <> "\n--- source >>> ---\n"
      <> Utils.emphasizeWhitespaces source
      <> "\n--- <<< source ---"
      <> "\n--- <<< error ---"

compatibilityTestCase
  ∷ String → String → Spec Unit
compatibilityTestCase title source = it
  ("understands \"" <> title <> "\"")
  do
    let
      actual = Program.run source
    case actual of
      Left errorMessage →
        fail $
          "--- error >>> ---\n"
            <> show errorMessage
            <> "\n--- source >>> ---\n"
            <> Utils.emphasizeWhitespaces source
            <> "\n--- <<< source ---"
            <> "\n--- <<< error ---"

      Right _ →
        pure unit

