{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module MonadicParsing () where

import Control.Applicative ()
import Data.Char (isAscii, isAsciiUpper, toLower)

data SomeType = Function deriving (Show)

newtype Parser a = P {runParse :: String -> [(a, String)]}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g f =
    P $ \inp -> case runParse f inp of
      [] -> []
      [(v, out)] -> [(g v, out)]

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P $ \inp -> [(v, inp)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P $ \inp -> case runParse pg inp of
      [] -> []
      [(g, out)] -> runParse (fmap g px) out

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \inp -> case runParse p inp of
    [] -> []
    [(v, out)] -> runParse (f v) out

myParser :: Parser SomeType
myParser = P $ \case
  ('f' : 'u' : 'n' : 'c' : 't' : 'i' : 'o' : 'n' : xs) -> [(Function, xs)]
  _ -> []