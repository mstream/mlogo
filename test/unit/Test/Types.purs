module Test.Types (TestSpec) where

import Prelude

import Effect.Aff (Aff)
import Test.Spec (SpecT)

type TestSpec = SpecT Aff Unit Aff Unit
