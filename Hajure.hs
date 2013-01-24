
module Main where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.Maybe (listToMaybe, fromMaybe)
import System.Environment
import System.IO

import ApplicativeParsec (ParseError)
import Hajure.AST
import Hajure.Data
import Hajure.Parsing

type ParseResult = Either ParseError (Element String)

main :: IO ()
main = getFilePath <$> getArgs >>= parseFile 

getFilePath :: [FilePath] -> FilePath
getFilePath = fromMaybe noFile . listToMaybe
  where noFile = error "No file given!"

parseFile :: FilePath -> IO ()
parseFile fp = withFile fp ReadMode (printResult . parse <=< hGetContents)

parse :: String -> ParseResult
parse = fmap listify . parseHajure

printResult :: ParseResult -> IO ()
printResult = either print printSExpr

printSExpr :: Show a => Element a -> IO ()
printSExpr (Nested sexpr) = print sexpr
printSExpr e              = print e

