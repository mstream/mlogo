module MLogo.Lexing
  ( asteriskSymbol
  , endKeyword
  , equalSymbol
  , falseKeyword
  , ifElseKeyword
  , ifKeyword
  , lexer
  , makeKeyword
  , plusSymbol
  , repeatKeyword
  , toKeyword
  , trueKeyword
  ) where

import Data.Identity (Identity)
import Parsing.Language as PL
import Parsing.String.Basic as PSB
import Parsing.Token (GenLanguageDef(..), GenTokenParser, LanguageDef)
import Parsing.Token as PT

asteriskSymbol ∷ String
asteriskSymbol = "*"

endKeyword ∷ String
endKeyword = "end"

equalSymbol ∷ String
equalSymbol = "="

falseKeyword ∷ String
falseKeyword = "false"

ifElseKeyword ∷ String
ifElseKeyword = "ifelse"

ifKeyword ∷ String
ifKeyword = "if"

makeKeyword ∷ String
makeKeyword = "make"

plusSymbol ∷ String
plusSymbol = "+"

repeatKeyword ∷ String
repeatKeyword = "repeat"

toKeyword ∷ String
toKeyword = "to"

trueKeyword ∷ String
trueKeyword = "true"

languageDef ∷ LanguageDef
languageDef = LanguageDef (PT.unGenLanguageDef PL.emptyDef)
  { identLetter = PSB.alphaNum
  , identStart = PSB.letter
  , reservedNames =
      [ ifKeyword
      , endKeyword
      , falseKeyword
      , ifElseKeyword
      , ifKeyword
      , makeKeyword
      , repeatKeyword
      , toKeyword
      , trueKeyword
      ]
  , reservedOpNames = [ asteriskSymbol, equalSymbol, plusSymbol ]
  }

lexer ∷ GenTokenParser String Identity
lexer = PT.makeTokenParser languageDef

