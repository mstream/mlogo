module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (List(..), (:))
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

type Line = { p1 :: Position, p2 :: Position }

data Command
  = Backward Steps
  | ClearScreen
  | Forward Steps
  | Home
  | Left Angle
  | PenDown
  | PenUp
  | Right Angle

type PointerState =
  { angle :: Angle
  , isDown :: Boolean
  , position :: Position
  }

type ScreenState = List Line

type ExecutionState =
  { pointer :: PointerState
  , screen :: ScreenState
  }

interpret :: Array Command -> ExecutionState
interpret = foldl f
  { pointer:
      { angle: zero
      , isDown: true
      , position: zero
      }
  , screen: Nil
  }
  where
  moveTo acc target = acc
    { pointer = acc.pointer { position = target }
    , screen =
        if acc.pointer.isDown then
          { p1: acc.pointer.position
          , p2: target
          } : acc.screen
        else acc.screen
    }

  f acc = case _ of
    Backward steps ->
      f acc $ Forward $ -steps
    ClearScreen ->
      acc { screen = Nil }
    Forward (Steps n) ->
      let
        d = toNumber n
        rads = toRadians acc.pointer.angle
        target = acc.pointer.position
          { x = acc.pointer.position.x + d * sin rads
          , y = acc.pointer.position.y + d * cos rads
          }
      in
        moveTo acc target
    Home ->
      moveTo acc { x: zero, y: zero }
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
    , ClearScreen
    , Right $ Angle 5
    , Home
    , Forward $ Steps 10
    , PenUp
    ]
