module Test.S where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Test.Mocha

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Values

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Data.Either
import Data.Foldable
import Data.Tuple
import Control.Apply
import Control.Alt
import Control.Alternative


assert x = do
  if not x then
    throwException $ error $ "error in assertion"
    else return unit

rawTextTest = do
  describe "correct raw text" $ do
    it "should parse 'abcdefфыва1234'" $ do
      assert $ case runParser "abcdefфыва1234" rawString of
        Right "abcdefфыва1234" -> true
        _ -> false
    it "should not parse '@34'" $ do
      assert $ case runParser "@34" rawString of
        Right x -> false
        Left y -> true 


slashedTest = do
  describe "slashed parser" $ do
    it "should parse '\t' as '\t'" $ do
      assert $ case runParser "\\t" slashed of
        Right "\\t" -> true
        _ -> false
    it "should not parse 't'" $ do
      assert $ case runParser "t" slashed of
        Left _ -> true
        Right _ -> false
    it "should parse '\"'" $ do
      assert $ case runParser "\\\"" slashed of
        Left _ -> false
        Right _ -> true


quotedTest = do
  describe "quotedString parser" $ do
    it "should be able to parse \"foo\"" $ do
      let res = case runParser "\"foo\"" quotedString of
            Right "\"foo\"" -> true
            _ -> false
      assert res
    it "should be able to parse \"foo\\\"\"" $ do
      assert $ case runParser "\"foo\\\"\"" quotedString of
        Right "\"foo\\\"\"" -> true
        _ -> false

    it "should not be able to parse string ended with '\'" $ do
      assert $ case runParser "foo\\" quotedString of
        Left _ -> true
        Right _ -> false

    it "should not be able to parse unquoted '\"'" $ do
      assert $ case runParser "foo\"bar" quotedString of
        Left _ -> true
        Right _ -> false

tokenTest = do
  describe "token" $ do
    it "should to do something" $ do
      let input = "foo = bar~:: *** \"foo\\\"bar\"" 
      let res :: Either ParseError [Token]
          res = runParser input tokenize 
      let expected = [
            Text("foo"),Eq,Text("bar"),Tilde,Colon,Colon,
            Star, Star, Star, Text("\"foo\\\"bar\"")]
      let toShow :: Either ParseError [Token]
          toShow = Right expected
      assert $ case res of
        Right tokens -> tokens == expected
        Left _ -> false
    it "should produce ne, lt, lte, gt, gte, and so on" $ do
      let input = "> <> >= != = < <= >="
          res = runParser input tokenize
          expected = [Gt, Ne, GtE, Ne, Eq, Lt, LtE, GtE]
      assert $ case res of
        Right tokens -> tokens == expected
        Left _ -> false


valueTest = do
  describe "vals parser" $ do
    it "should pass Eq, Ne through" $ do
      let input = [Eq, Ne]
      let expected = [Through Eq, Through Ne]
      let actual = runParser input vals
      assert $ case actual of
        Left _ -> false
        Right res -> res == expected
    it "should work with glob, range, etc" $ do
      let input = [Text "foo", Star, Text "bar",QMark, Text "1", 
                   Hash, Text "baz",
                   Text "baz", Colon,
                   At, Text "meta", Colon,
                   Text "quux",
                   Ne,
                   Text "foo", Range, Text "bar"]
      let expected = [Glob "foo*bar?1",
                      Tag "baz",
                      Label "baz",
                      MetaLabel "meta",
                      TextVal "quux",
                      Through Ne,
                      RangeVal "foo" "bar"]
      let actual = runParser input vals

      assert $ case actual of
        Left _ -> false
        Right res -> res == expected


searchTest = do
  describe "searchQuery" $ do
    it "should do something" $ do
      let inputs = [
            ">2",
            "foo",
            "+foo",
            "-foo",
            "#foo",
            "*",
            "uni*",
            "foo:>2",
            "foo:0..2",
            "-foo:0..2",
            "foo:bar:baz",
            "baz:~\"_foo%bar\"",
            "~?foo*bar",
            "foo:uni*",
            "@path:/foo/bar",
            "foo bar baz:quux:0..2"
            ]

      let results = [
            [IncludeTerm (SearchTermSimple [] (GtPredicate(TextVal("2"))))],
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo"))))],
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo"))))],
            [ExcludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo"))))],
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(Tag("foo"))))],
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(Glob("*"))))],
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(Glob("uni*"))))],
            [IncludeTerm (SearchTermSimple [Common("foo")] (GtPredicate(TextVal "2")))],
            [IncludeTerm (SearchTermSimple
                          [Common("foo")]
                          (ContainsPredicate(RangeVal "0" "2")))],
            
            [ExcludeTerm (SearchTermSimple
                          [Common("foo")]
                          (ContainsPredicate(RangeVal "0" "2")))],
            
            [IncludeTerm (SearchTermSimple 
                        [Common("foo"), Common("bar")]
                        (ContainsPredicate(TextVal("baz"))))],
            
            [IncludeTerm (SearchTermSimple
                          [Common("baz")]
                          (LikePredicate(TextVal("\"_foo%bar\""))))],
            
            [IncludeTerm (SearchTermSimple [] (LikePredicate(Glob("?foo*bar"))))],
            [IncludeTerm (SearchTermSimple
                          [Common("foo")]
                          (ContainsPredicate(Glob("uni*"))))],
            
            [IncludeTerm (SearchTermSimple
                          [Meta("path")]
                          (ContainsPredicate(TextVal("/foo/bar"))))],
            
            [IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo")))),
             IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("bar")))),
             IncludeTerm (SearchTermSimple 
                         [Common("baz"), Common("quux")]
                         (ContainsPredicate(RangeVal "0" "2")))]
            ]
      let cases = zip inputs results
      for_ cases $ \(Tuple input expected) ->
                   case parseSearchQuery input of
                     Left msg -> assert false
                     Right actual -> do
                       assert $ actual == expected



spec = do
  rawTextTest
  slashedTest
  quotedTest
  tokenTest
  valueTest
  searchTest

