{-# LANGUAGE LambdaCase #-}

module MyLib () where

data ParseError = ParseError
  { errExpected :: String,
    errFound :: String
  }
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> (String, Either ParseError a)
  }

any :: Parser Char
any = Parser $ \case
  (x : xs) -> (xs, Right x)
  [] -> ("", Left $ ParseError "Expected char" "got empty string")

eof :: Parser ()
eof = Parser $ \case
  [] -> ("", Right ())
  input@(c : _) -> (input, Left $ ParseError "Expected EOF" [c])

andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen parserA f = Parser $ \input ->
  case runParser parserA input of
    (restOfInput, Right a) -> runParser (f a) restOfInput
    (restOfInput, Left e) -> (restOfInput, Left e)