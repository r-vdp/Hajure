{-# OPTIONS_GHC -Wall #-}

module Hajure where

import Control.Monad
import Data.List
import Numeric

import ApplicativeParsec


data Element a = Nested (SExpr a) | Ident a | Num a | Op a
  deriving Show

newtype SExpr a = SExpr { unwrap :: [Element a] }

instance Show a => Show (SExpr a) where
  show (SExpr xs) = "S( " ++ showElements xs ++ " )"
    where showElements = join . intersperse " " . embrace
          embrace      = map (\x -> "{" ++ show x ++ "}")


parseHajure :: String -> Either ParseError (Element String)
parseHajure = parse sexpr ""

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) = liftA2 (++)

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

identifier :: Parser (Element String)
identifier = Ident <$> (many1 letter <++> many (alphaNum <|> char '_'))

number :: Parser (Element String)
number = Num . show <$> do
  s <- getInput
  case readSigned readFloat s of
    [(n,s')] -> n <$ setInput s'
    _        -> empty

operator :: Parser (Element String)
operator = Op . pure <$> (   char '+'
                         <|> char '-'
                         <|> char '*'
                         <|> char '/'
                         )

element :: Parser (Element String)
element = identifier
      <|> number
      <|> sexpr

sexpr :: Parser (Element String)
sexpr = Nested . SExpr <$> sexprFormat body
  where body  = start <:> rest
        start = operator <|> element
        rest  = separators1 *> element `sepEndBy` separators1

sexprFormat :: Parser a -> Parser a
sexprFormat   = between (open <* separators) close
  where open  = char '('
        close = char ')'

separator :: Parser Char
separator = space <|> newline

separators :: Parser String
separators = many separator

separators1 :: Parser String
separators1 = many1 separator

