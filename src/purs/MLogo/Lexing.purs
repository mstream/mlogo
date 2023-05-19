module MLogo.Lexing
  ( asteriskSymbol
  , caretSymbol
  , endKeyword
  , equalSymbol
  , falseKeyword
  , forKeyword
  , ifElseKeyword
  , ifKeyword
  , lexer
  , makeKeyword
  , minusSymbol
  , plusSymbol
  , repeatKeyword
  , reservedNames
  , slashSymbol
  , toKeyword
  , trueKeyword
  ) where

import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Parsing (ParserT)
import Parsing as P
import Parsing.Combinators ((<?>), (<??>))
import Parsing.Combinators as PC
import Parsing.Language as PL
import Parsing.String as PS
import Parsing.String.Basic as PSB
import Parsing.Token (GenLanguageDef(..), GenTokenParser, LanguageDef)
import Parsing.Token as PT

lexer ∷ GenTokenParser String Identity
lexer = lexer'
  { float = float <?> "float"
  , stringLiteral = stringLiteral <?> "string literal"
  }
  where
  lexer' ∷ GenTokenParser String Identity
  lexer' = PT.makeTokenParser languageDef

  stringLiteral ∷ ParserT String Identity String
  stringLiteral = PS.string "\"" *> do
    chars ← PC.many PSB.alphaNum
    pure
      $ String.fromCodePointArray
      $ Array.fromFoldable
      $ String.codePointFromChar <$> chars

  float ∷ ParserT String Identity Number
  float = do
    f ← PC.option identity sign
    mbDecimal ← PC.optionMaybe lexer'.decimal
    mbPeriod ← PC.lookAhead $ PC.optionMaybe $ PS.string "."
    x ← case mbDecimal, mbPeriod of
      Just decimal, _ →
        fractExponent decimal
      Nothing, Just _ →
        fractExponent 0
      Nothing, Nothing →
        P.fail "invalid float"
    pure $ f x

  fractExponent ∷ Int → ParserT String Identity Number
  fractExponent n = fractExponent' <|> justExponent
    where
    fractExponent' ∷ ParserT String Identity Number
    fractExponent' = do
      fract ← fraction
      expo ← PC.option 1.0 exponent'
      pure $ (Int.toNumber n + fract) * expo

    justExponent ∷ ParserT String Identity Number
    justExponent = do
      expo ← exponent'
      pure $ (Int.toNumber n * expo)

  fraction ∷ ParserT String Identity Number
  fraction = "fraction" <??> do
    _ ← PS.char '.'
    digits ← Array.some PSB.digit <?> "fraction"
    Maybe.maybe (P.fail "not digit") pure $ foldr op (Just 0.0) digits
    where
    op ∷ Char → Maybe Number → Maybe Number
    op _ Nothing = Nothing
    op d (Just f) = do
      int' ← Unicode.hexDigitToInt $ String.codePointFromChar d
      pure $ (f + Int.toNumber int') / 10.0

  exponent' ∷ ParserT String Identity Number
  exponent' = "exponent" <??> do
    _ ← PSB.oneOf [ 'e', 'E' ]
    f ← sign
    e ← lexer'.decimal <?> "exponent"
    pure $ power (f e)
    where
    power ∷ Int → Number
    power e
      | e < 0 = 1.0 / power (-e)
      | otherwise = 10.0 `Number.pow` Int.toNumber e

  sign ∷ ∀ a. Ring a ⇒ ParserT String Identity (a → a)
  sign = (PS.char '-' $> negate)
    <|> (PS.char '+' $> identity)
    <|> pure identity

languageDef ∷ LanguageDef
languageDef = LanguageDef (PT.unGenLanguageDef PL.emptyDef)
  { identLetter = PSB.alphaNum
  , identStart = PSB.letter
  , reservedNames = Array.fromFoldable reservedNames
  , reservedOpNames = reservedOpNames
  }

reservedNames ∷ Set String
reservedNames = Set.fromFoldable
  [ ifKeyword
  , endKeyword
  , falseKeyword
  , forKeyword
  , ifElseKeyword
  , ifKeyword
  , makeKeyword
  , repeatKeyword
  , toKeyword
  , trueKeyword
  ]

reservedOpNames ∷ Array String
reservedOpNames =
  [ asteriskSymbol
  , caretSymbol
  , equalSymbol
  , minusSymbol
  , plusSymbol
  , slashSymbol
  ]

asteriskSymbol ∷ String
asteriskSymbol = "*"

caretSymbol ∷ String
caretSymbol = "^"

endKeyword ∷ String
endKeyword = "end"

equalSymbol ∷ String
equalSymbol = "="

falseKeyword ∷ String
falseKeyword = "false"

forKeyword ∷ String
forKeyword = "for"

ifElseKeyword ∷ String
ifElseKeyword = "ifelse"

ifKeyword ∷ String
ifKeyword = "if"

makeKeyword ∷ String
makeKeyword = "make"

minusSymbol ∷ String
minusSymbol = "-"

plusSymbol ∷ String
plusSymbol = "+"

repeatKeyword ∷ String
repeatKeyword = "repeat"

slashSymbol ∷ String
slashSymbol = "/"

toKeyword ∷ String
toKeyword = "to"

trueKeyword ∷ String
trueKeyword = "true"

