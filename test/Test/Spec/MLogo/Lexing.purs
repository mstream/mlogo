module Test.Spec.MLogo.Lexing where

import Prelude
{-
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
      "an integer"
      "10"
      ( Right
          [ IntegerToken 10
          ]
      )

    testCase
      "a number"
      "10.0"
      ( Right
          [ NumberToken 10.0
          ]
      )

    testCase
      "a single command"
      "forward 10"
      ( Right
          [ UnquotedWord "forward"
          , IntegerToken 10
          ]
      )

    testCase
      "an output statement"
      "output 10"
      ( Right
          [ UnquotedWord "output"
          , IntegerToken 10
          ]
      )

    testCase
      "a command with a question mark suffix"
      "equal? 1 2"
      ( Right
          [ UnquotedWord "equal?"
          , IntegerToken 1
          , IntegerToken 2
          ]
      )

    testCase
      "single command, redundant spaces"
      " forward  10 "
      ( Right
          [ UnquotedWord "forward"
          , IntegerToken 10
          ]
      )

    testCase
      "multiple procedure invocations with various arguments count"
      "proc1 :var1\nproc2 :var1 :var2\nproc3 :var1 :var2 :var3"
      ( Right
          [ UnquotedWord "proc1"
          , ColonPrefixedWord "var1"
          , LineBreak
          , UnquotedWord "proc2"
          , ColonPrefixedWord "var1"
          , ColonPrefixedWord "var2"
          , LineBreak
          , UnquotedWord "proc3"
          , ColonPrefixedWord "var1"
          , ColonPrefixedWord "var2"
          , ColonPrefixedWord "var3"
          ]
      )

    testCase
      "a list of words"
      "[ word1 word2 ]"
      ( Right
          [ Bracket SquareOpening
          , UnquotedWord "word1"
          , UnquotedWord "word2"
          , Bracket SquareClosing
          ]
      )

    testCase
      "a list of words without a padding"
      "[word1 word2]"
      ( Right
          [ Bracket SquareOpening
          , UnquotedWord "word1"
          , UnquotedWord "word2"
          , Bracket SquareClosing
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
          , LineBreak
          , UnquotedWord "proc2"
          , ColonPrefixedWord "param1"
          , LineBreak
          , UnquotedWord "proc3"
          , ColonPrefixedWord "param2"
          , LineBreak
          , UnquotedWord "end"
          ]
      )

testCase ∷ String → String → ParseError \/ Array Token → Spec Unit
testCase title source expected = it
  ("lexes \"" <> title <> "\"")
  ((Lexing.run source) `shouldEqual` (List.fromFoldable <$> expected))
-}
