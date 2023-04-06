module Test.Spec.MLogo.Lexing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (BracketType(..), Token(..))
import MLogo.Lexing as Lexing
import StringParser (ParseError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Lexing" do
  describe "run" do

    testCase
      "example1"
      "forward 10\n"
      ( Right
          [ UnquotedWord "forward"
          , Number 10
          ]
      )

    testCase
      "example2"
      ( "( aaa111 111 \"aaa ) ;aaa\n"
          <> "[ bbb222 222 222 \"bbb ] ;bbb\n"
          <> "{ ccc333 }\n"
          <> "to proc1 :param1 :param2\n"
          <> "end"
      )
      ( Right $
          [ Bracket RoundOpening
          , UnquotedWord "aaa111"
          , Number 111
          , QuotedWord "aaa"
          , Bracket RoundClosing
          , Comment "aaa"
          , Bracket SquareOpening
          , UnquotedWord "bbb222"
          , Number 222
          , Number 222
          , QuotedWord "bbb"
          , Bracket SquareClosing
          , Comment "bbb"
          , Bracket CurlyOpening
          , UnquotedWord "ccc333"
          , Bracket CurlyClosing
          , UnquotedWord "to"
          , UnquotedWord "proc1"
          , ColonPrefixedWord "param1"
          , ColonPrefixedWord "param2"
          , UnquotedWord "end"
          ]
      )

testCase :: String -> String -> ParseError \/ Array Token -> Spec Unit
testCase title source expected = it
  ("lexes \"" <> title <> "\"")
  ((Lexing.run source) `shouldEqual` (List.fromFoldable <$> expected))

