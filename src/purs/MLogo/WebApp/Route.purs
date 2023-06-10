module MLogo.WebApp.Route
  ( Route(..)
  , genRoute
  , parse
  , print
  ) where

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
import MLogo.WebApp.BaseUrl (BaseUrl)
import MLogo.WebApp.BaseUrl as BaseUrl
import MLogo.WebApp.Utils as Utils
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Routing.Duplex.Generic.Syntax ((?))
import Routing.Duplex.Parser (RouteError)

data Route
  = Home
  | Sandbox { s ∷ Maybe String }
  | StaticAsset String

derive instance Generic Route _
derive instance Eq Route

instance Show Route where
  show = genericShow

type Source = String

routeWithBaseOf ∷ BaseUrl → RouteDuplex' Route
routeWithBaseOf = BaseUrl.segments >>> case _ of
  [] →
    D.root route
  baseSegments →
    D.root $ D.path (String.joinWith "/" baseSegments) route
  where
  route ∷ RouteDuplex' Route
  route = DG.sum
    { "Home": homeRoute
    , "Sandbox": sandboxRoute
    , "StaticAsset": D.string D.segment
    }

homeRoute ∷ RouteDuplex' NoArguments
homeRoute = DG.noArgs

sandboxRoute ∷ RouteDuplex' { s ∷ Maybe String }
sandboxRoute = "sandbox" ? { s: D.optional <<< source }

source ∷ RouteDuplex' String → RouteDuplex' Source
source = D.as toString fromString
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

parse ∷ BaseUrl → String → RouteError \/ Route
parse baseUrl s = case String.stripPrefix (Pattern baseUrlPrefix) s of
  Just "" →
    Right Home
  _ →
    case String.stripSuffix (Pattern "/") s of
      Just "" →
        Right Home
      Just s' →
        D.parse (routeWithBaseOf baseUrl) s'
      Nothing →
        D.parse (routeWithBaseOf baseUrl) s
  where
  baseUrlPrefix ∷ String
  baseUrlPrefix = String.joinWith "/" (BaseUrl.segments baseUrl)

print ∷ BaseUrl → Route → String
print = D.print <<< routeWithBaseOf

genRoute ∷ ∀ m. Alt m ⇒ MonadGen m ⇒ MonadRec m ⇒ m Route
genRoute = genHome <|> genSandbox

genHome ∷ ∀ m. MonadGen m ⇒ m Route
genHome = pure Home

genSandbox ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Route
genSandbox = do
  s ← GenCommon.genMaybe StringGen.genAlphaString
  pure $ Sandbox { s }

