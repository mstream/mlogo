module MLogo.Lexing
  ( asteriskSymbol
  , endKeyword
  , equalSymbol
  , falseKeyword
  , forKeyword
  , ifElseKeyword
  , ifKeyword
  , lexer
  , makeKeyword
  , plusSymbol
  , repeatKeyword
  , reservedNames
  , toKeyword
  , trueKeyword
  ) where

import Data.Identity (Identity)
import Parsing.Language as PL
import Parsing.String.Basic as PSB
import Parsing.Token (GenLanguageDef(..), GenTokenParser, LanguageDef)
import Parsing.Token as PT

lexer ∷ GenTokenParser String Identity
lexer = PT.makeTokenParser languageDef

languageDef ∷ LanguageDef
languageDef = LanguageDef (PT.unGenLanguageDef PL.emptyDef)
  { identLetter = PSB.alphaNum
  , identStart = PSB.letter
  , reservedNames = reservedNames
  , reservedOpNames = [ asteriskSymbol, equalSymbol, plusSymbol ]
  }

reservedNames ∷ Array String
reservedNames =
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

asteriskSymbol ∷ String
asteriskSymbol = "*"

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

plusSymbol ∷ String
plusSymbol = "+"

repeatKeyword ∷ String
repeatKeyword = "repeat"

toKeyword ∷ String
toKeyword = "to"

trueKeyword ∷ String
trueKeyword = "true"

