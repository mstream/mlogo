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
  , plusSymbol
  , repeatKeyword
  , reservedNames
  , slashSymbol
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
  , reservedOpNames = reservedOpNames
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

reservedOpNames ∷ Array String
reservedOpNames =
  [ asteriskSymbol, caretSymbol, equalSymbol, plusSymbol, slashSymbol ]

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

