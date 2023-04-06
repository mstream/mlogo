module MLogo.Lexing
  ( BracketType(..)
  , Token(..)
  , run
  ) where

import Prelude

import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List ((:), List)
import Data.Maybe (Maybe(..))
import Data.String as String
import StringParser (ParseError, Parser)
import StringParser as SP

data Token
  = ColonPrefixedWord String
  | Comment String
  | Bracket BracketType
  | Number Int
  | QuotedWord String
  | UnquotedWord String

derive instance Eq Token

instance Show Token where
  show = case _ of
    ColonPrefixedWord s ->
      "ColonPrefixedWord \"" <> s <> "\""
    Comment s ->
      "Comment \"" <> s <> "\""
    Bracket b ->
      "Bracket \"" <> show b <> "\""
    Number n ->
      "Number \"" <> show n <> "\""
    QuotedWord s ->
      "QuotedWord \"" <> s <> "\""
    UnquotedWord s ->
      "UnquotedWord \"" <> s <> "\""

data BracketType
  = CurlyClosing
  | CurlyOpening
  | RoundClosing
  | RoundOpening
  | SquareClosing
  | SquareOpening

derive instance Eq BracketType

instance Show BracketType where
  show = case _ of
    CurlyClosing ->
      "}"
    CurlyOpening ->
      "{"
    RoundClosing ->
      ")"
    RoundOpening ->
      "("
    SquareClosing ->
      "]"
    SquareOpening ->
      "["

run :: String -> ParseError \/ List Token
run = SP.runParser programParser

programParser :: Parser (List Token)
programParser = tokenParser `SP.sepBy`
  ( SP.choice
      [ SP.string " "
      , SP.string "\n"
      ]
  )

tokenParser :: Parser Token
tokenParser = SP.choice
  [ bracketParser
  , colonPrefixedWordParser
  , commentParser
  , numberParser
  , quotedWordParser
  , unquotedWordParser
  ]

bracketParser :: Parser Token
bracketParser = Bracket <$> SP.choice
  [ CurlyClosing <$ SP.string "}"
  , CurlyOpening <$ SP.string "{"
  , RoundClosing <$ SP.string ")"
  , RoundOpening <$ SP.string "("
  , SquareClosing <$ SP.string "]"
  , SquareOpening <$ SP.string "["
  ]

colonPrefixedWordParser :: Parser Token
colonPrefixedWordParser = do
  void $ SP.string ":"
  letter <- SP.anyLetter
  alphaNums <- SP.many SP.alphaNum
  pure $ ColonPrefixedWord $ charsToString $ letter : alphaNums

commentParser :: Parser Token
commentParser = do
  void $ SP.string ";"
  chars <- SP.many $ SP.satisfy (_ /= '\n')
  pure $ Comment $ charsToString chars

quotedWordParser :: Parser Token
quotedWordParser = do
  void $ SP.string "\""
  letter <- SP.anyLetter
  alphaNums <- SP.many SP.alphaNum
  pure $ QuotedWord $ charsToString $ letter : alphaNums

unquotedWordParser :: Parser Token
unquotedWordParser = do
  letter <- SP.anyLetter
  alphaNums <- SP.many SP.alphaNum
  pure $ UnquotedWord $ charsToString $ letter : alphaNums

numberParser :: Parser Token
numberParser = do
  digits <- SP.many1 SP.anyDigit
  let
    s = charsToString digits
  case Int.fromString s of
    Just n ->
      pure $ Number n
    Nothing ->
      SP.fail
        $ "\"" <> s <> "\" is not a valid number"

charsToString :: forall f. Foldable f => Functor f => f Char -> String
charsToString = String.fromCodePointArray
  <<< Array.fromFoldable
  <<< map String.codePointFromChar
