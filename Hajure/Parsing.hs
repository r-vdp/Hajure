
module Hajure.Parsing (parseHajure) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Read

import ApplicativeParsec
import Hajure.Data

parseHajure :: Text -> Either ParseError TextElem
parseHajure = parse sexpr ""

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) = liftA2 (++)

identifier :: Parser TextElem
identifier = Ident . T.concat <$> (many1 (T.singleton <$> letter) <++> many identifierChar)

identifierChar :: Parser Text
identifierChar = T.singleton <$> (alphaNum <|> char '_' <|> char '\'')

number :: Parser TextElem
number = Num <$> (getInput >>= parseNum)
  where parseNum       = either doLeft doRight . signed double
        doRight (n,s') = packFloat n <$ setInput s'
        doLeft  _      = empty

packFloat :: RealFloat a => a -> Text
packFloat = toLazyText . realFloat

operator :: Parser TextElem
operator = Op . T.singleton <$> (char '+'
                            <|>  char '-'
                            <|>  char '*'
                            <|>  char '/'
                            )

list :: Parser TextElem
list = List <$> between' open close separators (element `sepEndBy` separators1)
  where open  = char '['
        close = char ']'

element :: Parser TextElem
element = identifier
      <|> number
      <|> operator
      <|> list
      <|> sexpr

sexpr :: Parser TextElem
sexpr = Nested . SExpr <$> sexprFormat body
  where body  = element `sepEndBy` separators1

sexprFormat :: Parser a -> Parser a
sexprFormat   = between' open close separators
  where open  = char '('
        close = char ')'

between' :: Parser open -> Parser close -> Parser sep -> Parser a -> Parser a
between' open close sep = between (open <* sep) close

separator :: Parser Text
separator = T.singleton <$> (space <|> newline)

separators :: Parser Text
separators = T.concat <$> many separator

separators1 :: Parser Text
separators1 = T.concat <$> many1 separator

