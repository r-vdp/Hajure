
module Main where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import System.Environment
import System.IO hiding (hGetContents)

import ApplicativeParsec (ParseError)
import Hajure.AST
import Hajure.Data
import Hajure.Parsing

type ParseResult = Either ParseError Element

main :: IO ()
main = parseFile =<< getFilePath <$> getArgs

getFilePath :: [FilePath] -> FilePath
getFilePath = fromMaybe noFile . listToMaybe
  where noFile = error "No file given!"

parseFile :: FilePath -> IO ()
parseFile fp = withFile fp ReadMode (printResult . parse <=< hGetContents)

parse :: Text -> ParseResult
parse = fmap listify . parseHajure

printResult :: ParseResult -> IO ()
printResult = either print printSExpr

printSExpr :: Element -> IO ()
printSExpr (Nested sexpr) = print sexpr
printSExpr e              = print e

