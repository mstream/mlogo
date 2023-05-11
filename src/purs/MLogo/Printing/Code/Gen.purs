module MLogo.Printing.Code.Gen (genCode) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy)
import Control.Lazy as Lazy
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.String.Gen as StringGen
import MLogo.Printing.Code (Code(..), CodeWord(..))

genCode ∷ ∀ m. Alt m ⇒ MonadGen m ⇒ MonadRec m ⇒ Lazy (m Code) ⇒ m Code
genCode = Gen.resize (min 3) (Gen.sized go)
  where
  go ∷ Int → m Code
  go size = if size == 0 then genLeaf else Gen.resize (_ - 1) genBranch

  genLeaf ∷ m Code
  genLeaf = genSingleLine

  genBranch ∷ m Code
  genBranch = genIndented <|> genMultiLine

  genIndented ∷ m Code
  genIndented = Indented <$> Lazy.defer \_ → genCode

  genMultiLine ∷ m Code
  genMultiLine = MultiLine <$> Gen.unfoldable (Lazy.defer \_ → genCode)

  genSingleLine ∷ m Code
  genSingleLine = SingleLine <$> Gen.unfoldable genCodeWord

genCodeWord ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m CodeWord
genCodeWord = CodeWord <$> StringGen.genAlphaString
