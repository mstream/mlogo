module Test.Spec.MLogo.WebApp (spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..))
import Data.String as String
import Examples (Example(..))
import Examples as Examples
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Parsing as Parsing
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , ParameterName(..)
  , UnaryOperationType(..)
  )
import MLogo.Parsing.Expression.Gen as ExpressionGen
import MLogo.Printing as Printing
import MLogo.Printing.Code (codeToString) as Code
import Parsing (ParseError)
import Parsing as P
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen as Gen
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.MLogo.WebApp.Route (spec) as Route
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)
import Test.Utils as Utils

spec ∷ TestSpec
spec = describe "WebApp" do
  Route.spec
