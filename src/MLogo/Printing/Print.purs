module MLogo.Printing.Print (Print) where

import MLogo.Printing.Code (Code)

type Print a = a → Int → Code

