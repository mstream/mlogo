module Test.Spec.MLogo.Program (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.String as String
import MLogo.Interpretation.State (VisibleState)
import MLogo.Interpretation.State.Color as Color
import MLogo.Program as Program
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Types (TestSpec)
import Test.Utils as Utils

spec ∷ TestSpec
spec = describe "Program" do
  describe "run" do

    testCase
      "empty program"
      ""
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
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
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 10.0 }
                }
              ]
          }
      )

    testCase
      "moving backward by 10 using the forward command"
      ( String.joinWith
          "\n"
          [ "forward -10" ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: -10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: -10.0 }
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
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 10.0 }
                }
              ]
          }
      )

    testCase
      "moving forward by 10 using a custom procedure to retrieve the number of steps"
      ( String.joinWith
          "\n"
          [ "to steps"
          , "output 10"
          , "end"
          , "fd steps"
          ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 10.0 }
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
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 5.0 }
                , p2: { x: 0.0, y: 10.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 5.0 }
                }
              ]
          }
      )

    testCase
      "moving forward by 10 using the repeat loop block"
      ( String.joinWith
          "\n"
          [ "repeat 2 [ forward 5 ]"
          ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 5.0 }
                , p2: { x: 0.0, y: 10.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 5.0 }
                }
              ]
          }
      )

    testCase
      "moving forward multiple times using the repeat loop block and its repcount"
      ( String.joinWith
          "\n"
          [ "repeat 3 [ forward repcount ]"
          ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 6.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 3.0 }
                , p2: { x: 0.0, y: 6.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 1.0 }
                , p2: { x: 0.0, y: 3.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 1.0 }
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
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 10.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 10.0 }
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
              , color: Color.black
              , isDown: true
              , position: zero
              }
          , screen: Nil
          }
      )

    testCase
      "moving forward multiple times using a for loop block"
      ( String.joinWith
          "\n"
          [ "for [i 4 6] [fd :i]" ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 15.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 9.0 }
                , p2: { x: 0.0, y: 15.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 4.0 }
                , p2: { x: 0.0, y: 9.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 4.0 }
                }
              ]
          }
      )

    testCase
      "moving forward multiple times using a for loop block with a custom step"
      ( String.joinWith
          "\n"
          [ "for [i 1 6 2] [fd :i]" ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 9.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 4.0 }
                , p2: { x: 0.0, y: 9.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 1.0 }
                , p2: { x: 0.0, y: 4.0 }
                }
              , { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 1.0 }
                }
              ]
          }
      )

    testCase
      "moving forward depending on a lexical scope of the for loop"
      ( String.joinWith
          "\n"
          [ "for [i 1 1] [for [j 2 2] [for [k 3 3] [fd :i + :j + :k]]]"
          ]
      )
      ( Right $
          { pointer:
              { angle: zero
              , color: Color.black
              , isDown: true
              , position: { x: 0.0, y: 6.0 }
              }
          , screen: List.fromFoldable
              [ { color: Color.black
                , p1: { x: 0.0, y: 0.0 }
                , p2: { x: 0.0, y: 6.0 }
                }
              ]
          }
      )

testCase
  ∷ String → String → String \/ VisibleState → TestSpec
testCase title source expected = it ("executes \"" <> title <> "\"") do
  let
    actual = Program.run 0 source
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

