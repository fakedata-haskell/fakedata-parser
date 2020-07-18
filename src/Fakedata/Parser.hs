module Fakedata.Parser where

import Control.Applicative
import Control.Monad (void)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as T

-- | Type representing the unresolved text
data FakeIRValue
  = Literal Text
  | Hash Int
  | Ques Int
  | Resolve Text
  deriving (Show, Eq)

parseLiteralTextSome :: P.Parser FakeIRValue
parseLiteralTextSome = do
  literal <- some $ P.satisfy (not . isHashOrQues)
  pure $ Literal $ T.pack literal

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

parseQuesSome :: P.Parser FakeIRValue
parseQuesSome = do
  ques <- some $ P.char '?'
  pure $ Ques (Prelude.length ques)

singleton :: a -> [a]
singleton x = [x]

parseFakedata :: P.Parser [FakeIRValue]
parseFakedata = do
  xs <- many $ ((singleton <$> parseLiteralTextSome) <|> parseHashSome <|> (singleton <$> parseQuesSome))
  pure $ concat xs

isHashOrQues :: Char -> Bool
isHashOrQues '?' = True
isHashOrQues '#' = True
isHashOrQues _ = False
