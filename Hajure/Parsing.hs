
module Hajure.Parsing (parseHajure) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read

import Hajure.Data
import ParsecImports

-- $setup
-- >>> :set -XOverloadedStrings

-- |
-- >>> let i = "(+ 2 3 [test 2])" :: Text
-- >>> let o = SExpr [Op "+", Num 2, Num 3, List [Ident "test", Num 2]]
-- >>> either (error . show) (== [o]) $ parseHajure i
-- True
parseHajure :: Text -> Either ParseError [SExpr]
parseHajure = parse parser ""
  where parser = separators *> sexpr `sepEndBy` separators <* eof

(<:>) :: Applicative f => f Char -> f Text -> f Text
(<:>) = liftA2 T.cons

identifier :: Parser Element
identifier = Ident <$> identifierHead <:> identifierTail

identifierHead :: Parser Char
identifierHead = letter <|> char '_'

identifierTail :: Parser Text
identifierTail = T.pack <$> many (identifierHead <|> digit <|> char '\'')

number :: Parser Element
number = Num <$> (getInput >>= parseNum)
  where parseNum       = either doLeft doRight . signed double
        doRight (n,s') = n <$ setInput s'
        doLeft  _      = empty

operator :: Parser Element
operator = Op . T.singleton <$> (char '+'
                            <|>  char '-'
                            <|>  char '*'
                            <|>  char '/'
                            )

list :: Parser Element
list = List <$> between' open close separators elements
  where open     = char '['
        close    = char ']'
        elements = element `sepEndBy` separators1

element :: Parser Element
element = identifier
      <|> number
      <|> operator
      <|> list
      <|> nestedSExpr

sexpr :: Parser SExpr
sexpr = SExpr <$> sexprFormat body
  where body  = element `sepEndBy` separators1

nestedSExpr :: Parser Element
nestedSExpr = Nested <$> sexpr

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

