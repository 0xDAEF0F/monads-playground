{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module MonadicParsing () where

import Control.Applicative ()
import Data.Char ()

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g f =
    P
      ( \inp -> case parse f inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \case
  [] -> []
  (x : xs) -> [(x, xs)]

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g x y z = (x, z)