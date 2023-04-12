module MLogo.Lexing
  ( BracketType(..)
  , Token(..)
  , run
  ) where

import Prelude

import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String as String
import StringParser (ParseError, Parser)
import StringParser as SP

data Token
  = Bracket BracketType
  | ColonPrefixedWord String
  | Comment String
  | IntegerToken Int
  | LineBreak
  | NumberToken Number
  | QuotedWord String
  | UnquotedWord String

derive instance Generic Token _
derive instance Eq Token

instance Show Token where
  show = genericShow

data BracketType
  = CurlyClosing
  | CurlyOpening
  | RoundClosing
  | RoundOpening
  | SquareClosing
  | SquareOpening

derive instance Generic BracketType _
derive instance Eq BracketType

instance Show BracketType where
  show = genericShow

run ∷ String → ParseError \/ List Token
run = SP.runParser programParser

programParser ∷ Parser (List Token)
programParser =
  (skipSpaces *> tokenParser) `SP.sepEndBy` skipSpaces
  where
  skipSpaces = void $ SP.many $ SP.string " "

tokenParser ∷ Parser Token
tokenParser = SP.choice
  [ LineBreak <$ SP.string "\n"
  , bracketParser
  , colonPrefixedWordParser
  , commentParser
  , numericTokenParser
  , quotedWordParser
  , unquotedWordParser
  ]

bracketParser ∷ Parser Token
bracketParser = Bracket <$> SP.choice
  [ CurlyClosing <$ SP.string "}"
  , CurlyOpening <$ SP.string "{"
  , RoundClosing <$ SP.string ")"
  , RoundOpening <$ SP.string "("
  , do
      SP.skipSpaces
      void $ SP.string "]"
      pure SquareClosing
  , do
      void $ SP.string "["
      SP.skipSpaces
      pure SquareOpening
  ]

commentParser ∷ Parser Token
commentParser = do
  void $ SP.string ";"
  chars ← SP.many $ SP.satisfy (_ /= '\n')
  pure $ Comment $ charsToString chars

wordParser ∷ Parser String
wordParser = do
  letter ← SP.anyLetter
  alphaNums ← SP.many SP.alphaNum
  mbSuffix ← SP.optionMaybe $ SP.string "?"
  let
    prefix = charsToString $ letter : alphaNums
  pure case mbSuffix of
    Nothing →
      prefix
    Just suffix →
      prefix <> suffix

colonPrefixedWordParser ∷ Parser Token
colonPrefixedWordParser = do
  void $ SP.string ":"
  word ← wordParser
  pure $ ColonPrefixedWord word

quotedWordParser ∷ Parser Token
quotedWordParser = do
  void $ SP.string "\""
  word ← wordParser
  pure $ QuotedWord word

unquotedWordParser ∷ Parser Token
unquotedWordParser = do
  word ← wordParser
  pure $ UnquotedWord word

numericTokenParser ∷ Parser Token
numericTokenParser = do
  digits ← SP.many1 $ SP.choice [ SP.anyDigit, SP.char '.' ]
  let
    s = charsToString digits
  case Int.fromString s of
    Just n →
      pure $ IntegerToken n
    Nothing →
      case Number.fromString s of
        Just x →
          pure $ NumberToken x
        Nothing →
          SP.fail
            $ "\"" <> s <> "\" is not a numeric token"

charsToString ∷ ∀ f. Foldable f ⇒ Functor f ⇒ f Char → String
charsToString = String.fromCodePointArray
  <<< Array.fromFoldable
  <<< map String.codePointFromChar
