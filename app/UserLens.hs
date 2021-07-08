{-# LANGUAGE RankNTypes #-}
module UserLens (UserLens (..), getUserLens, toLens) where

import Arguments

import Control.Lens
import qualified Data.Aeson.Lens
import Data.Aeson.Types (Value)
import Data.Text
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data UserLens = Nth Int
              | Values
              | Key String
              deriving Show

int :: Parser Int
int = do
  n <- many1 digit
  return $ read n

nth :: Parser UserLens
nth = do
  char '['
  n <- int
  char ']'
  return $ Nth n

values :: Parser UserLens
values = do
  char '['
  char ']'
  return Values

key :: Parser UserLens
key = do
  char '.'
  first <- letter <|> char '@'
  rest <- many $ alphaNum <|> char '_' <|> char '/' <|> char '-'
  return $ Key $ first : rest

userLens :: Parser UserLens
userLens = try values <|> nth <|> key

getUserLens :: String -> Result UserLens
getUserLens str = case parse userLens "" str of {
  Left err -> Left $ show err;
  Right val -> Right val
  }

single :: UserLens -> Traversal' Value Value
single (Nth n) = Data.Aeson.Lens.nth n
single Values = Data.Aeson.Lens.values
single (Key k) = Data.Aeson.Lens.key $ pack k

toLens :: [UserLens] -> Traversal' String Value
toLens args = Prelude.foldl (.) Data.Aeson.Lens._Value (fmap single args)
