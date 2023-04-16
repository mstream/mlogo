module MLogo.Parsing (Expression(..), expression) where

import Prelude

import Control.Lazy as Lazy
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import MLogo.Lexing as Lexing
import Parsing (Parser)
import Parsing.Combinators as PC
import Parsing.Expr (Assoc(..), Operator(..))
import Parsing.Expr as PE
import Parsing.String as PS
import Parsing.String.Basic as PSB

data Expression
  = NumberLiteral Number
  | Multiplication Expression Expression

derive instance Generic Expression _

derive instance Eq Expression

instance Show Expression where
  show s = genericShow s

expression ∷ Parser String Expression
expression = PSB.skipSpaces *> PE.buildExprParser operatorTable term
  where
  operatorTable =
    [ [ Infix
          ( Multiplication <$ PC.between
              (PC.optional PSB.skipSpaces)
              (PC.optional PSB.skipSpaces)
              (PS.string "*")
          )
          AssocRight
      ]
    ]
  term = PC.choice
    [ Lazy.defer \_ → Lexing.tokenParser.parens expression
    , NumberLiteral <$> Lexing.tokenParser.float
    ]

{-
keywords ∷ Set String
keywords = Set.fromFoldable
  [ "end"
  , "if"
  , "ifelse"
  , "output"
  , "repeat"
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
  | ExpressionStatement Expression
  | ProcedureCall String (List Statement)
  | ProcedureDefinition String (List Parameter) (List Statement)

derive instance Generic Statement _
derive instance Eq Statement

instance Show Statement where
  show statement = genericShow statement

instance Arbitrary Statement where
  arbitrary = Lazy.defer \_ → genericArbitrary

newtype Parameter = Parameter String

derive newtype instance Eq Parameter
derive newtype instance Ord Parameter
derive newtype instance Show Parameter
derive newtype instance Arbitrary Parameter

data Expression
  = BooleanLiteral Boolean
  | ListLiteral (List Statement)
  | NumericLiteralExpression NumericLiteral
  | VariableReference String
  | WordLiteral String

derive instance Generic Expression _
derive instance Eq Expression

instance Show Expression where
  show expression = genericShow expression

instance Arbitrary Expression where
  arbitrary = Lazy.defer \_ → genericArbitrary

data NumericLiteral
  = IntegerLiteral Int
  | NumberLiteral Number

derive instance Generic NumericLiteral _
derive instance Eq NumericLiteral

instance Show NumericLiteral where
  show = genericShow

instance Arbitrary NumericLiteral where
  arbitrary = genericArbitrary

data ControlStructure
  = IfBlock Statement (List Statement)
  | IfElseBlock Statement (List Statement) (List Statement)
  | OutputCall Statement
  | RepeatBlock Statement (List Statement)

derive instance Generic ControlStructure _
derive instance Eq ControlStructure

instance Show ControlStructure where
  show = genericShow

instance Arbitrary ControlStructure where
  arbitrary = Lazy.defer \_ → genericArbitrary

procedureCallParser ∷ TokenParser Statement
procedureCallParser = Lazy.defer \_ → do
  name ← consumeUnquotedWord isNotKeyword
  args ← P.many procedureArgumentParser
  pure $ ProcedureCall name args

procedureArgumentParser ∷ TokenParser Statement
procedureArgumentParser = Lazy.defer \_ → P.choice
  [ expressionStatementParser
  , P.between
      (consumeBracket (_ == RoundOpening))
      (consumeBracket (_ == RoundClosing))
      procedureCallParser
  ]

run ∷ List Token → ParseError \/ List Statement
run tokens = P.runParser tokens programParser

programParser ∷ ProgramParser
programParser = Lazy.defer \_ →
  statementParser `P.sepBy` skipLineBreaks

statementParser ∷ TokenParser Statement
statementParser = Lazy.defer \_ →
  P.choice
    [ controlStructureStatementParser
    , expressionStatementParser
    , procedureDefinitionParser
    , procedureCallParser
    ]

expressionStatementParser ∷ TokenParser Statement
expressionStatementParser = ExpressionStatement <$> expressionParser

controlStructureStatementParser ∷ TokenParser Statement
controlStructureStatementParser = Lazy.defer \_ →
  ControlStructureStatement <$> P.choice
    [ ifBlockParser
    , ifElseBlockParser
    , outputCallParser
    , repeatBlockParser
    ]

ifBlockParser ∷ TokenParser ControlStructure
ifBlockParser = do
  void $ consumeUnquotedWord (_ == "if")
  consumeBracket (_ == RoundOpening)
  conditionExpression ← statementParser
  consumeBracket (_ == RoundClosing)
  consumeBracket (_ == SquareOpening)
  positiveBranch ← programParser
  consumeBracket (_ == SquareClosing)
  pure $ IfBlock conditionExpression positiveBranch

ifElseBlockParser ∷ TokenParser ControlStructure
ifElseBlockParser = do
  void $ consumeUnquotedWord (_ == "ifelse")
  consumeBracket (_ == RoundOpening)
  conditionExpression ← statementParser
  consumeBracket (_ == RoundClosing)
  consumeBracket (_ == SquareOpening)
  positiveBranch ← P.many statementParser
  consumeBracket (_ == SquareClosing)
  consumeBracket (_ == SquareOpening)
  negativeBranch ← P.many statementParser
  consumeBracket (_ == SquareClosing)
  pure $ IfElseBlock conditionExpression positiveBranch negativeBranch

outputCallParser ∷ TokenParser ControlStructure
outputCallParser = do
  void $ consumeUnquotedWord (_ == "output")
  expression ← statementParser
  pure $ OutputCall expression

repeatBlockParser ∷ TokenParser ControlStructure
repeatBlockParser = do
  void $ consumeUnquotedWord (_ == "repeat")
  timesExpression ← statementParser
  consumeBracket (_ == SquareOpening)
  body ← P.many statementParser
  consumeBracket (_ == SquareClosing)
  pure $ RepeatBlock timesExpression body

procedureDefinitionParser ∷ TokenParser Statement
procedureDefinitionParser = do
  void $ consumeUnquotedWord (_ == "to")
  name ← consumeUnquotedWord isNotKeyword
  parameters ← P.many consumeColonPrefixedWord
  skipLineBreaks
  body /\ _ ← P.manyTill_
    ( do
        statement ← statementParser
        void $ P.optional skipLineBreaks
        pure statement
    )
    (consumeUnquotedWord (_ == "end"))
  pure $ ProcedureDefinition name (Parameter <$> parameters) body

skipLineBreaks ∷ TokenParser Unit
skipLineBreaks =
  void $ P.many skipLineBreak
  where
  skipLineBreak ∷ TokenParser Unit
  skipLineBreak = consumeToken case _ of
    LineBreak →
      Just unit
    _ →
      Nothing

expressionParser ∷ TokenParser Expression
expressionParser = Lazy.defer \_ →
  P.choice
    [ booleanLiteralParser
    , numericLiteralExpressionParser
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

numericLiteralExpressionParser ∷ TokenParser Expression
numericLiteralExpressionParser = NumericLiteralExpression <$> P.choice
  [ IntegerLiteral <$> consumeIntegerToken
  , NumberLiteral <$> consumeNumberToken
  ]

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

consumeIntegerToken ∷ TokenParser Int
consumeIntegerToken = consumeToken case _ of
  IntegerToken x →
    Just x
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
                (ParseError ("unexpected token " <> show head) pos)
  )

-}
