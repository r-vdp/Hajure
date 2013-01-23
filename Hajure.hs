
module Main where
  
import System.Environment
import System.IO

import ApplicativeParsec (ParseError)
import Hajure.AST
import Hajure.Data
import Hajure.Parsing

main :: IO ()
main = do
  (x:_) <- getArgs
  withFile x ReadMode (\h -> either print print . parse =<< hGetContents h)

parse :: String -> Either ParseError (Element String)
parse = fmap listify . parseHajure

