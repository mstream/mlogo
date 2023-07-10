module Test.Benchmark.Main where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, for_)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import MLogo.Interpretation.State (VisibleState)
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing
import MLogo.Printing.Code as Code
import MLogo.Program as Program
import MLogo.Program.Example (Example(..))
import MLogo.Program.Examples as Examples
import Performance.Minibench as MB

main ∷ Effect Unit
main = for_
  Examples.functional
  \(Example { ast }) → benchProgram ast

bench ∷ ∀ a. String → (Unit → a) → Effect Unit
bench name f = do
  Console.info $ "---\n" <> name <> "\n---"
  MB.bench f
  Console.info "---"

benchProgram ∷ ∀ f. Foldable f ⇒ Functor f ⇒ f Expression → Effect Unit
benchProgram ast = case thunk unit of
  Left errorMessage →
    throw $ "program has failed: " <> errorMessage
  Right _ →
    bench source thunk
  where
  thunk ∷ Unit → String \/ VisibleState
  thunk = \_ → Program.run 0 source

  source ∷ String
  source = Code.codeToString $ Printing.printExpressions
    ast
    { pageWidth: 50, simplifyBinaryOperations: true }
