{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Main where

import Control.Applicative
import Data.Char
import Distribution.Simple.CCompiler (filenameCDialect)

-- JSON data types, AST
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: geen floats
  | JsonString String
  | JsonArray [JsonValue] -- NOTE: recursief, herhaald zichzelf
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq) -- NOTE: Show is nodig om het printable te maken

-- Parser; NOTE: no proper error reporting
-- runParser is een structure
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- NOTE: 'prove' to compiler that Parser is a Functor
-- NOTE: penetration operations
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

-- NOTE: Pure operation, double penetration
-- NOTE: Chaining parsers, <*> chains parsers together
instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

-- NOTE: check if Boolean
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (head : tail)
      | head == x = Just (tail, x)
      | otherwise = Nothing
    f [] = Nothing

-- NOTE: sequenceA turns inside out
stringP :: String -> Parser String
stringP = sequenceA . map charP

-- NOTE: Alternative interface nodig
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    -- NOTE: This should never happen!
    f _ = undefined

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

-- NOTE: Take a parser wrap it in another parser
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNumber $ read ds

-- NOTE: no escape support
stringLiteral :: Parser String
stringLiteral = charP '"' *> (spanP (/= '"')) <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

-- whitespace
ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray
    <$> ( charP '['
            *> ws
            *> elements
            <* ws
            <* charP ']'
        )
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair =
      (\key _ value -> (key, value))
        <$> stringLiteral
        <*> (ws *> charP ':' <* ws)
        <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

-- NOTE: help functie voor het lezen van een input JSON file
parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined