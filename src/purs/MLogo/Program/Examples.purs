module MLogo.Program.Examples (fancy, functional, all) where

import Data.Map (Map)
import Data.Map as Map
import MLogo.Program.Example (Example)
import MLogo.Program.Examples.Fancy as Fancy
import MLogo.Program.Examples.Functional as Functional

all ∷ Map String Example
all = Map.union fancy functional

fancy ∷ Map String Example
fancy = Fancy.examplesByTitle

functional ∷ Map String Example
functional = Functional.examplesByTitle

