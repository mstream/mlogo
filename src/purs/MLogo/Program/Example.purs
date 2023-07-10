module MLogo.Program.Example (Example(..), ToMap(..)) where

import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import MLogo.Parsing.Expression (Expression)
import Type.Proxy (Proxy)

newtype Example = Example
  { ast ∷ Array Expression
  , source ∷ String
  }

data ToMap = ToMap

instance
  ( IsSymbol sym
  ) ⇒
  FoldingWithIndex
    ToMap
    (Proxy sym)
    (Map String Example)
    Example
    (Map String Example) where
  foldingWithIndex ToMap prop acc val =
    Map.insert (reflectSymbol prop) val acc

