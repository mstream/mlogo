module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Number (cos, pi, sin)
import Effect (Effect)
import Effect.Console (log)

newtype Angle = Angle Int

derive newtype instance Show Angle
derive newtype instance Semiring Angle
derive newtype instance Ring Angle

toRadians :: Angle -> Number
toRadians (Angle n) = toNumber n * pi / 180.0

newtype Steps = Steps Int

derive newtype instance Semiring Steps
derive newtype instance Ring Steps

type Position = { x :: Number, y :: Number }

data Command
  = Backward Steps
  | Forward Steps
  | Left Angle
  | PenDown
  | PenUp
  | Right Angle

type PointerState =
  { angle :: Angle
  , isDown :: Boolean
  , position :: Position
  }

type ExecutionState =
  { pointer :: PointerState
  }

interpret :: Array Command -> ExecutionState
interpret = foldl f
  { pointer:
      { angle: zero
      , isDown: true
      , position: zero
      }
  }
  where
  f acc = case _ of
    Backward steps ->
      f acc $ Forward $ -steps
    Forward (Steps n) ->
      acc
        { pointer = acc.pointer
            { position = acc.pointer.position
                { x = acc.pointer.position.x + (toNumber n) * (sin $ toRadians acc.pointer.angle)
                , y = acc.pointer.position.y + (toNumber n) * (cos $ toRadians acc.pointer.angle)
                }
            }
        }
    Left angle ->
      f acc $ Right $ -angle
    PenDown ->
      acc { pointer = acc.pointer { isDown = true } }
    PenUp ->
      acc { pointer = acc.pointer { isDown = false } }
    Right angle ->
      acc { pointer = acc.pointer { angle = acc.pointer.angle + angle } }

main :: Effect Unit
main = do
  log $ show $ interpret
    [ Forward $ Steps 10
    , Right $ Angle 5
    , Forward $ Steps 10
    , PenUp
    ]
