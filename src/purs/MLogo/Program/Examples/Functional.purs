module MLogo.Program.Examples.Functional (examplesByTitle) where

import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Heterogeneous.Folding as Heterogeneous
import MLogo.Parsing.Expression (Expression(..))
import MLogo.Program.Example (Example(..), ToMap(..))

examplesByTitle ∷ Map String Example
examplesByTitle =
  Heterogeneous.hfoldlWithIndex
    ToMap
    (Map.empty ∷ Map String Example)
    { "single command": Example
        { ast:
            [ ProcedureCall "forward"
                (List.fromFoldable [ IntegerLiteral 10 ])
            ]
        , source: "forward 10"
        }
    , "for loop": Example
        { ast:
            [ ForBlock
                { binder: "i"
                , initialValue: 1
                , step: 1
                , terminalValue: 1000
                }
                ( List.fromFoldable
                    [ ProcedureCall "forward"
                        (List.fromFoldable [ ValueReference "i" ])
                    ]
                )
            ]
        , source: ""
        }
    }

