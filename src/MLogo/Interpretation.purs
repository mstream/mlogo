module MLogo.Interpretation (run) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import MLogo.Parsing (Expression(..), Parameter(..), ProcedureCall(..), Statement(..))
import MLogo.Interpretation.State (ExecutionState)
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Statement as Statement

run :: List Statement -> String \/ ExecutionState
run = foldM Statement.interpret State.initialExecutionState

{-
run :: List Statement -> String \/ ExecutionState
run = foldl f
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
        d = Int.toNumber n
        rads = toRadians acc.pointer.angle
        target = acc.pointer.position
          { x = acc.pointer.position.x + d * Number.sin rads
          , y = acc.pointer.position.y + d * Number.cos rads
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
-}
