module MLogo.WebApp.Route (Route(..), genRoute, parse, print) where

import Prelude hiding ((/))

import Control.Alt (class Alt)
import Control.Alternative ((<|>))
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen.Common as GenCommon
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Gen as StringGen
import MLogo.WebApp.Utils as Utils
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Duplex
import Routing.Duplex.Generic as DuplexGeneric
import Routing.Duplex.Generic.Syntax ((?))
import Routing.Duplex.Parser (RouteError(..))

data Route = Home | Sandbox { s ∷ Maybe String }

derive instance Generic Route _
derive instance Eq Route

instance Show Route where
  show = genericShow

type Source = String

route ∷ RouteDuplex' Route
route = DuplexGeneric.sum
  { "Home": homeRoute
  , "Sandbox": sandboxRoute
  }

homeRoute ∷ RouteDuplex' NoArguments
homeRoute = DuplexGeneric.noArgs

sandboxRoute ∷ RouteDuplex' { s ∷ Maybe String }
sandboxRoute = "sandbox" ? { s: Duplex.optional <<< source }

source ∷ RouteDuplex' String → RouteDuplex' Source
source = Duplex.as toString fromString
  where
  fromString ∷ String → String \/ Source
  fromString = note "non-decodable"
    <<< Utils.decodeUriComponentFromString

  toString ∷ Source → String
  toString = Utils.encodeToUriComponent >>> case _ of
    Nothing →
      ""
    Just uriEncodedString →
      Utils.uriEncodedStringToString uriEncodedString

parse ∷ String → String → RouteError \/ Route
parse basePath = String.stripPrefix (Pattern basePath) >>> case _ of
  Nothing →
    Left $ Expected "base path" basePath
  Just s →
    Duplex.parse route s

print ∷ String → Route → String
print basePath = (basePath <> _) <<< Duplex.print route

genRoute ∷ ∀ m. Alt m ⇒ MonadGen m ⇒ MonadRec m ⇒ m Route
genRoute = genHome <|> genSandbox

genHome ∷ ∀ m. MonadGen m ⇒ m Route
genHome = pure Home

genSandbox ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Route
genSandbox = do
  s ← GenCommon.genMaybe StringGen.genAlphaString
  pure $ Sandbox { s }
