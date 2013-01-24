
module Hajure.Parsing (parseHajure) where

import Numeric

import ApplicativeParsec
import Hajure.Data

parseHajure :: String -> Either ParseError (Element String)
parseHajure = parse sexpr ""

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) = liftA2 (++)

identifier :: Parser (Element String)
identifier = Ident <$> (many1 letter <++> many identifierChar)

identifierChar :: Parser Char
identifierChar = alphaNum <|> char '_' <|> char '\''

number :: Parser (Element String)
number = Num . show <$> do
    s <- getInput
    case doRead s of
      [(n,s')] -> n <$ setInput s'
      _        -> empty
  where
    doRead :: ReadS Double
    doRead = readSigned readFloat

operator :: Parser (Element String)
operator = Op . pure <$> (   char '+'
                         <|> char '-'
                         <|> char '*'
                         <|> char '/'
                         )

list :: Parser (Element String)
list = List <$> between' open close separators (element `sepEndBy` separators1)
  where open  = char '['
        close = char ']'

element :: Parser (Element String)
element = identifier
      <|> operator
      <|> number
      <|> list
      <|> sexpr

sexpr :: Parser (Element String)
sexpr = Nested . SExpr <$> sexprFormat body
  where body  = element `sepEndBy` separators1

sexprFormat :: Parser a -> Parser a
sexprFormat   = between' open close separators
  where open  = char '('
        close = char ')'

between' :: Parser open -> Parser close -> Parser sep -> Parser a -> Parser a
between' open close sep = between (open <* sep) close

separator :: Parser Char
separator = space <|> newline

separators :: Parser String
separators = many separator

separators1 :: Parser String
separators1 = many1 separator

