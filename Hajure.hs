{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hajure where

import Data.Function
import Numeric

import ApplicativeParsec


newtype SExpr a = SExpr { unwrap :: [a] }
  deriving (Show, Read, Functor, Applicative, Monad)

type SExprParser a = Parser (SExpr a)

append :: SExpr a -> SExpr a -> SExpr a
append = fmap SExpr . (++) `on` unwrap

cons :: a -> SExpr a -> SExpr a
cons a s = SExpr (a : unwrap s)

parseHajure :: String -> Either ParseError (SExpr String)
parseHajure = parse hajure ""

hajure :: SExprParser String
hajure = sexpr operator number

sexpr :: Parser String -> Parser String -> SExprParser String
sexpr = list (char '(') (char ')')

list :: Parser open   ->
        Parser close  ->
        Parser String ->
        Parser String ->
        SExprParser String
list open close h t =
  between (open <* spaces) close $
    cons <$> (h <* spaces) <*> (SExpr <$> (t `sepEndBy` spaces))

number :: Parser String
number = show <$> do
  s <- getInput
  case readSigned readFloat s of
    [(n,s')] -> n <$ setInput s'
    _        -> empty

operator :: Parser String
operator = pure <$> (   char '+'
                    <|> char '-'
                    <|> char '*'
                    <|> char '/'
                    )

