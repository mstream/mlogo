module MLogo.Printing.Print (Print, PrintConfig) where

import MLogo.Printing.Code (Code)

type PrintConfig =
  { pageWidth ∷ Int
  , simplifyBinaryOperations ∷ Boolean
  }

type Print a = a → PrintConfig → Code

