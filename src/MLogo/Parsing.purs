module MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
  , Parameter(..)
  , ProcedureCall(..)
  , Statement(..)
  , run
  ) where

import Prelude

import Control.Lazy as Lazy
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (mkFn5, runFn2)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import MLogo.Lexing (BracketType(..), Token(..))
import Parsing
  ( ParseError(..)
  , ParseState(..)
  , Parser
  , ParserT(..)
  , Position(..)
  )
import Parsing (fail, runParser) as P
import Parsing.Combinators (choice, many) as P

keywords ∷ Set String
keywords = Set.fromFoldable
  [ "end"
  , "if"
  , "to"
  ]

isKeyword ∷ String → Boolean
isKeyword s = Set.member s keywords

isNotKeyword ∷ String → Boolean
isNotKeyword = not <<< isKeyword

type TokenParser a = Parser (List Token) a
type ProgramParser = TokenParser (List Statement)

data Statement
  = ControlStructureStatement ControlStructure
  | ProcedureCallStatement ProcedureCall
  | ProcedureDefinition String (List Parameter) (List Statement)

derive instance Generic Statement _
derive instance Eq Statement

instance Show Statement where
  show statement = genericShow statement

newtype Parameter = Parameter String

derive newtype instance Eq Parameter
derive newtype instance Ord Parameter
derive newtype instance Show Parameter

data Expression
  = BooleanLiteral Boolean
  | ListLiteral (List Expression)
  | NumericLiteral Number
  | ProcedureCallExpression ProcedureCall
  | VariableReference String
  | WordLiteral String

derive instance Generic Expression _
derive instance Eq Expression

instance Show Expression where
  show expression = genericShow expression

data ControlStructure
  = IfBlock Expression (List Statement)
  | IfElseBlock Expression (List Statement) (List Statement)

derive instance Generic ControlStructure _
derive instance Eq ControlStructure

instance Show ControlStructure where
  show = genericShow

data ProcedureCall = ProcedureCall String (List Expression)

derive instance Generic ProcedureCall _
derive instance Eq ProcedureCall

instance Show ProcedureCall where
  show = genericShow

procedureCallParser ∷ TokenParser ProcedureCall
procedureCallParser = Lazy.defer \_ → do
  name ← consumeUnquotedWord isNotKeyword
  args ← P.many expressionParser
  pure $ ProcedureCall name args

run ∷ List Token → ParseError \/ List Statement
run tokens = P.runParser tokens programParser

programParser ∷ ProgramParser
programParser = P.many statementParser

statementParser ∷ TokenParser Statement
statementParser = Lazy.defer \_ →
  P.choice
    [ controlStructureStatementParser
    , procedureCallStatementParser
    , procedureDefinitionParser
    ]

controlStructureStatementParser ∷ TokenParser Statement
controlStructureStatementParser = Lazy.defer \_ →
  ControlStructureStatement <$> P.choice
    [ ifBlockParser
    ]

ifBlockParser ∷ TokenParser ControlStructure
ifBlockParser = do
  void $ consumeUnquotedWord (_ == "if")
  condExpr ← expressionParser
  consumeBracket (_ == SquareOpening)
  body ← P.many statementParser
  consumeBracket (_ == SquareClosing)
  pure $ IfBlock condExpr body

procedureCallStatementParser ∷ TokenParser Statement
procedureCallStatementParser =
  ProcedureCallStatement <$> procedureCallParser

procedureDefinitionParser ∷ TokenParser Statement
procedureDefinitionParser = do
  void $ consumeUnquotedWord (_ == "to")
  name ← consumeUnquotedWord isNotKeyword
  params ← P.many consumeColonPrefixedWord
  body ← P.many statementParser
  void $ consumeUnquotedWord (_ == "end")
  pure $ ProcedureDefinition name (Parameter <$> params) body

expressionParser ∷ TokenParser Expression
expressionParser = Lazy.defer \_ →
  P.choice
    [ booleanLiteralParser
    , numericLiteralParser
    , procedureCallExpressionParser
    , variableReferenceParser
    , wordLiteralParser
    ]

booleanLiteralParser ∷ TokenParser Expression
booleanLiteralParser = do
  bool ← consumeUnquotedWord \s → s == "false" || s == "true"
  case bool of
    "false" →
      pure $ BooleanLiteral false
    "true" →
      pure $ BooleanLiteral true
    other →
      P.fail $ "\"" <> other <> "\" is not a boolean value"

numericLiteralParser ∷ TokenParser Expression
numericLiteralParser = NumericLiteral <$> consumeNumberToken

procedureCallExpressionParser ∷ TokenParser Expression
procedureCallExpressionParser = Lazy.defer \_ → do
  consumeBracket (_ == RoundOpening)
  procCall ← procedureCallParser
  consumeBracket (_ == RoundClosing)
  pure $ ProcedureCallExpression procCall

variableReferenceParser ∷ TokenParser Expression
variableReferenceParser = VariableReference <$> consumeColonPrefixedWord

wordLiteralParser ∷ TokenParser Expression
wordLiteralParser = WordLiteral <$> consumeQuotedWord

consumeQuotedWord ∷ TokenParser String
consumeQuotedWord = consumeToken case _ of
  QuotedWord s →
    Just s
  _ → Nothing

consumeUnquotedWord ∷ (String → Boolean) → TokenParser String
consumeUnquotedWord predicate = consumeToken case _ of
  UnquotedWord s →
    if predicate s then Just s else Nothing
  _ →
    Nothing

consumeColonPrefixedWord ∷ TokenParser String
consumeColonPrefixedWord = consumeToken case _ of
  ColonPrefixedWord s →
    Just s
  _ →
    Nothing

consumeNumberToken ∷ TokenParser Number
consumeNumberToken = consumeToken case _ of
  NumberToken x →
    Just x
  _ →
    Nothing

consumeBracket ∷ (BracketType → Boolean) → TokenParser Unit
consumeBracket predicate = consumeToken case _ of
  Bracket bt →
    if predicate bt then Just unit else Nothing
  _ →
    Nothing

consumeToken ∷ ∀ a. (Token → Maybe a) → TokenParser a
consumeToken f = ParserT
  ( mkFn5 \state@(ParseState input pos _) _ _ throw done →
      case List.uncons input of
        Nothing →
          runFn2 throw state (ParseError "Unexpected EOF" pos)
        Just { head, tail } →
          case f head of
            Just ast →
              let
                (Position p) = pos
              in
                runFn2 done
                  ( ParseState tail
                      (Position $ p { index = p.index + 1 })
                      true
                  )
                  ast
            Nothing →
              runFn2 throw state
                (ParseError "Predicate unsatisfied" pos)
  )

