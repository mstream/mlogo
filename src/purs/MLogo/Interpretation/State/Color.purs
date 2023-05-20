module MLogo.Interpretation.State.Color
  ( Color
  , Saturation
  , aqua
  , black
  , blue
  , brown
  , cyan
  , forest
  , genColor
  , green
  , grey
  , magenta
  , orange
  , purple
  , red
  , salmon
  , saturationFromInt
  , tan
  , toRGB
  , white
  , yellow
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))

type Color = { blue ∷ Saturation, green ∷ Saturation, red ∷ Saturation }

newtype Saturation = Saturation Int

derive newtype instance EncodeJson Saturation
derive newtype instance Eq Saturation
derive newtype instance Show Saturation

toRGB ∷ Color → { blue ∷ Int, green ∷ Int, red ∷ Int }
toRGB
  { blue: Saturation b
  , green: Saturation g
  , red: Saturation r
  } = { blue: b, green: g, red: r }

saturationFromInt ∷ Int → String \/ Saturation
saturationFromInt i
  | i < 0 = Left "saturation cannot be negative"
  | i > 100 = Left "saturation cannot be greater than 100"
  | otherwise = Right $ Saturation i

genColor ∷ ∀ m. MonadGen m ⇒ m Color
genColor = ado
  blue ← genSaturation
  green ← genSaturation
  red ← genSaturation
  in { blue, green, red }

genSaturation ∷ ∀ m. MonadGen m ⇒ m Saturation
genSaturation = Saturation <$> Gen.chooseInt 0 100

aqua ∷ Color
aqua =
  { blue: Saturation 100
  , green: Saturation 100
  , red: Saturation 0
  }

black ∷ Color
black =
  { blue: Saturation 0
  , green: Saturation 0
  , red: Saturation 0
  }

blue ∷ Color
blue =
  { blue: Saturation 100
  , green: Saturation 0
  , red: Saturation 0
  }

brown ∷ Color
brown =
  { blue: Saturation 0
  , green: Saturation 29
  , red: Saturation 59
  }

cyan ∷ Color
cyan =
  { blue: Saturation 100
  , green: Saturation 100
  , red: Saturation 0
  }

forest ∷ Color
forest =
  { blue: Saturation 13
  , green: Saturation 55
  , red: Saturation 13
  }

green ∷ Color
green =
  { blue: Saturation 0
  , green: Saturation 100
  , red: Saturation 0
  }

grey ∷ Color
grey =
  { blue: Saturation 50
  , green: Saturation 50
  , red: Saturation 50
  }

magenta ∷ Color
magenta =
  { blue: Saturation 100
  , green: Saturation 0
  , red: Saturation 100
  }

orange ∷ Color
orange =
  { blue: Saturation 0
  , green: Saturation 65
  , red: Saturation 100
  }

purple ∷ Color
purple =
  { blue: Saturation 50
  , green: Saturation 0
  , red: Saturation 50
  }

red ∷ Color
red =
  { blue: Saturation 0
  , green: Saturation 0
  , red: Saturation 100
  }

salmon ∷ Color
salmon =
  { blue: Saturation 45
  , green: Saturation 50
  , red: Saturation 98
  }

tan ∷ Color
tan =
  { blue: Saturation 55
  , green: Saturation 71
  , red: Saturation 82
  }

white ∷ Color
white =
  { blue: Saturation 100
  , green: Saturation 100
  , red: Saturation 100
  }

yellow ∷ Color
yellow =
  { blue: Saturation 0
  , green: Saturation 100
  , red: Saturation 100
  }

