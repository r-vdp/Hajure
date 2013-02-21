
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

identifier :: HParser Element
identifier = Ident <$> identifierHead <:> identifierTail

identifierHead :: HParser Char
identifierHead = letter <|> char '_'

identifierTail :: HParser Text
identifierTail = T.pack <$> many (identifierHead <|> digit <|> char '\'')

number :: HParser Element
number = Num <$> (getInput >>= parseNum)
  where parseNum       = either doLeft doRight . signed double
        doRight (n,s') = n <$ setInput s'
        doLeft  _      = empty

operator :: HParser Element
operator = Op . T.singleton <$> (char '+'
                            <|>  char '-'
                            <|>  char '*'
                            <|>  char '/'
                            )

list :: HParser Element
list = List <$> between' open close separators elements
  where open     = char '['
        close    = char ']'
        elements = element `sepEndBy` separators1

element :: HParser Element
element = identifier
      <|> number
      <|> operator
      <|> list
      <|> nestedSExpr

sexpr :: HParser SExpr
sexpr = SExpr <$> sexprFormat body
  where body  = element `sepEndBy` separators1

nestedSExpr :: HParser Element
nestedSExpr = Nested <$> sexpr

sexprFormat :: HParser a -> HParser a
sexprFormat   = between' open close separators
  where open  = char '('
        close = char ')'

between' :: HParser open -> Parser close -> Parser sep -> Parser a -> Parser a
between' open close sep = between (open <* sep) close

separator :: HParser Text
separator = T.singleton <$> (space <|> newline)

separators :: HParser Text
separators = T.concat <$> many separator

separators1 :: HParser Text
separators1 = T.concat <$> many1 separator

