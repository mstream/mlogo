module Test.Main where

import Prelude

import Data.Array ((!!))
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.Process as Process
import Test.Spec (Spec)
import Test.Spec.MLogo.Interpretation as Interpretation
import Test.Spec.MLogo.Lexing as Lexing
import Test.Spec.MLogo.Parsing as Parsing
import Test.Spec.MLogo.Printing as Printing
import Test.Spec.MLogo.Program as Program
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = do
  args ← Process.argv
  specs ← selectSpecs $ args !! 2
  launchAff_ $ runSpec [ consoleReporter ] (sequence_ specs)
  where
  selectSpecs ∷ Maybe String → Effect (Array (Spec Unit))
  selectSpecs = case _ of
    Nothing →
      pure allSpecs
    Just moduleName →
      case moduleName of
        "Interpretation" →
          pure [ Interpretation.spec ]
        "Lexing" →
          pure [ Lexing.spec ]
        "Parsing" →
          pure [ Parsing.spec ]
        "Printing" →
          pure [ Printing.spec ]
        "Program" →
          pure [ Program.spec ]
        _ →
          throw $ "Unknown module name \"" <> moduleName <> "\""

  allSpecs =
    [ Interpretation.spec
    , Lexing.spec
    , Parsing.spec
    , Printing.spec
    , Program.spec
    ]
