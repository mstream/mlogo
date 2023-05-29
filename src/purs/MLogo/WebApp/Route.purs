module MLogo.WebApp.Route (Route(..), genRoute, parse, print) where

import Prelude hiding ((/))

import Control.Alt (class Alt)
import Control.Alternative ((<|>))
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen.Common as GenCommon
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Gen as StringGen
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Duplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((?))
import Routing.Duplex.Parser (RouteError)
import Utils as Utils

data Route = Home | Sandbox { s ∷ Maybe String }

derive instance Generic Route _
derive instance Eq Route

instance Show Route where
  show = genericShow

type Source = String

route ∷ RouteDuplex' Route
route = Duplex.root $ sum
  { "Home": noArgs
  , "Sandbox": "sandbox.html" ? { s: Duplex.optional <<< source }
  }

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

parse ∷ String → RouteError \/ Route
parse = case _ of
  "" →
    Right Home
  s →
    Duplex.parse route s

print ∷ Route → String
print = Duplex.print route

genRoute ∷ ∀ m. Alt m ⇒ MonadGen m ⇒ MonadRec m ⇒ m Route
genRoute = genHome <|> genSandbox

genHome ∷ ∀ m. MonadGen m ⇒ m Route
genHome = pure Home

genSandbox ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Route
genSandbox = do
  s ← GenCommon.genMaybe StringGen.genAlphaString
  pure $ Sandbox { s }
