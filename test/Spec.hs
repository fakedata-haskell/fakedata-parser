{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Data.Attoparsec.Text as P
import Data.Text
import Lib
import Test.Hspec
import Control.Applicative

main :: IO ()
main =
  hspec $ do
    describe "parseLiteral" $ do
      it "parses resolved text" $ do
        let result = P.parseOnly parseLiteralTextSome "hello world"
        result `shouldBe` (Right $ Literal "hello world")
      it "parses resolved text and hash" $ do
        let result = P.parseOnly parseLiteralTextSome "hello world#"
        result `shouldBe` (Right $ Literal "hello world")
      it "parses resolved text and ?" $ do
        let result = P.parseOnly parseLiteralTextSome "hello wo?rld?#"
        result `shouldBe` (Right $ Literal "hello wo")
    describe "parseHashSome" $ do
      it "works with - #####" $ do
        let result = P.parseOnly parseHashSome "#####"
        result `shouldBe` (Right [ Hash 5 ])
      it "works with - ###{hello}" $ do
        let result = P.parseOnly parseHashSome "###{hello}"
        result `shouldBe` (Right [ Hash 2, Resolve "hello" ])
      it "works with - #{hello}" $ do
        let result = P.parseOnly parseHashSome "#{hello}"
        result `shouldBe` (Right [ Resolve "hello" ])
    describe "parseFakedata" $ do
      it "works with - hello world" $ do
        let result = P.parseOnly parseFakedata "hello world"
        result `shouldBe` (Right $ (Literal "hello world"):[])
      it "works with - hello world ###" $ do
        let result = P.parseOnly parseFakedata "hello world ###"
        result `shouldBe` (Right $ (Literal "hello world "):[Hash 3])
      it "works with - hello world ### ??" $ do
        let result = P.parseOnly parseFakedata "hello world ### ??"
        result `shouldBe` (Right $ (Literal "hello world "):[Hash 3, Literal " ", Ques 2])
      it "works with - ###" $ do
        let result = P.parseOnly parseFakedata "###"
        result `shouldBe` (Right $ (Hash 3):[])
      it "works with - ???" $ do
        let result = P.parseOnly parseFakedata "???"
        result `shouldBe` (Right $ (Ques 3):[])
      it "works with - ???hello" $ do
        let result = P.parseOnly parseFakedata "???hello"
        result `shouldBe` (Right $ (Ques 3):[Literal "hello"])
      it "works with - ???hello #{hello} hi #{world}" $ do
        let result = P.parseOnly parseFakedata "???hello #{hello} hi #{world}"
        result `shouldBe` (Right $ (Ques 3):[Literal "hello ", Resolve "hello", Literal " hi ", Resolve "world"])
    describe "unresolved text in address" $ do
      it "works with - PO Box ####" $ do
        let result = P.parseOnly parseFakedata "PO Box ####"
        result `shouldBe` (Right $ (Literal "PO Box "):[Hash 4])
      it "works with - #{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}" $ do
        let result = P.parseOnly parseFakedata "#{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}"
        result `shouldBe` (Right $ (Resolve "secondary_address"):[Hash 4])
