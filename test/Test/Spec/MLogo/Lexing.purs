module Test.Spec.MLogo.Lexing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Lexing (Token(..))
import MLogo.Lexing as Lexing
import StringParser (ParseError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Lexing" do
  describe "run" do

    testCase
      "single command"
      "forward 10"
      ( Right
          [ UnquotedWord "forward"
          , Number 10
          ]
      )

    testCase
      "single command, reduntant spaces"
      " forward  10 "
      ( Right
          [ UnquotedWord "forward"
          , Number 10
          ]
      )

    testCase
      "procedure definition"
      ( "to proc1 :param1 :param2\n"
          <> "proc2 :param1\n"
          <> "proc3 :param2\n"
          <> "end"
      )
      ( Right $
          [ UnquotedWord "to"
          , UnquotedWord "proc1"
          , ColonPrefixedWord "param1"
          , ColonPrefixedWord "param2"
          , UnquotedWord "proc2"
          , ColonPrefixedWord "param1"
          , UnquotedWord "proc3"
          , ColonPrefixedWord "param2"
          , UnquotedWord "end"
          ]
      )

testCase ∷ String → String → ParseError \/ Array Token → Spec Unit
testCase title source expected = it
  ("lexes \"" <> title <> "\"")
  ((Lexing.run source) `shouldEqual` (List.fromFoldable <$> expected))

