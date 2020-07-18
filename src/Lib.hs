module Lib where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as T

-- Assumptions of the parser. #{ will have #}
-- "PO Box ###{test} hi ??"
data FakeIRValue
  = Literal Text
  | Hash Int
  | Ques Int
  | Resolve Text
  deriving (Show, Eq)

data FakedataIR =
  FakedataIR
    { values :: [FakeIRValue]
    }
  deriving (Show, Eq)

parseLiteralText :: P.Parser FakeIRValue
parseLiteralText = do
  literal <- many $ P.satisfy (not . isHashOrQues)
  pure $ Literal $ T.pack literal

parseLiteralTextSome :: P.Parser FakeIRValue
parseLiteralTextSome = do
  literal <- some $ P.satisfy (not . isHashOrQues)
  pure $ Literal $ T.pack literal


parseHash :: P.Parser [FakeIRValue]
parseHash = do
  hashes <- many $ P.char '#'
  let numHahes = Prelude.length hashes
  nh <-
    case numHahes of
      0 -> pure Nothing
      n -> P.peekChar
  case nh of
    Nothing -> pure $ [Hash numHahes]
    Just c ->
      if c == '{'
        then do
          void P.anyChar
          xs <- P.takeTill (\a -> a == '}')
          void $ P.char '#'
          case numHahes of
            0 -> fail "parseHash: undefined state"
            1 -> pure $ [Resolve xs]
            n -> pure $ [Hash (numHahes - 1), Resolve xs]
        else pure $ [Hash numHahes]

parseHashSome :: P.Parser [FakeIRValue]
parseHashSome = do
  hashes <- some $ P.char '#'
  let numHahes = Prelude.length hashes
  nh <- P.peekChar
  case nh of
    Nothing -> pure $ [Hash numHahes]
    Just c ->
      if c == '{'
        then do
          void P.anyChar
          xs <- P.takeTill (\a -> a == '}')
          void P.anyChar
          case numHahes of
            0 -> fail "parseHash: undefined state"
            1 -> pure $ [Resolve xs]
            n -> pure $ [Hash (numHahes - 1), Resolve xs]
        else pure $ [Hash numHahes]

parseQues :: P.Parser FakeIRValue
parseQues = do
  ques <- many $ P.char '?'
  pure $ Ques (Prelude.length ques)

parseQuesSome :: P.Parser FakeIRValue
parseQuesSome = do
  ques <- some $ P.char '?'
  pure $ Ques (Prelude.length ques)

-- parseAllFakedata :: P.Parser [FakeIRValue]
-- parseAllFakedata = do
--   a <- (parseLiteralText <|> parseHash <|> parseQues)
--   b <- parseAllFakedata
--   pure $ (a:b)
singleton :: a -> [a]
singleton x = [x]

-- parseFakedata :: P.Parser [FakeIRValue]
-- parseFakedata = do
--   a <-
--     ((singleton <$> parseLiteralText) <|> parseHash <|>
--      (singleton <$> parseQues))
--   b <-
--     (parseHash <|> (singleton <$> parseLiteralText) <|>
--      (singleton <$> parseQues))
--   c <-
--     ((singleton <$> parseQues) <|> (singleton <$> parseLiteralText) <|>
--      parseHash)
--   pure $ a <> b <> c

parseFakedata :: P.Parser [FakeIRValue]
parseFakedata = do
  xs <- many $ ((singleton <$> parseLiteralTextSome) <|> parseHashSome <|> (singleton <$> parseQuesSome))
  pure $ concat xs

isHashOrQues :: Char -> Bool
isHashOrQues '?' = True
isHashOrQues '#' = True
isHashOrQues _ = False

-- parser :: Parser FakeIRValue
-- parser = do
--   literal <- takeWhile (not . isHashOrQues)
someFunc :: IO ()
someFunc = putStrLn "someFunc"
