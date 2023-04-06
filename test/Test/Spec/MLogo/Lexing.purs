module Test.Spec.MLogo.Lexing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List as List
import MLogo.Lexing (BracketType(..), Token(..))
import MLogo.Lexing as Lexing
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Lexing" do
  describe "run" do
    it "lexes example1" do
      let
        s = "( aaa111 111 \"aaa ) ;aaa\n"
          <> "[ bbb222 222 222 \"bbb ] ;bbb\n"
          <> "{ ccc333 }\n"
          <> "to proc1 :param1 :param2\n"
          <> "end"
        actual = Lexing.run s
        expected = Right $ List.fromFoldable
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

      actual `shouldEqual` expected

