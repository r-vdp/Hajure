
module Main where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad ((<=<))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import System.Environment
import System.IO hiding (hGetContents)

import Hajure.AST
import Hajure.Data
import Hajure.Parsing
import ParsecImports (ParseError)

type ParseResult = Either ParseError ([Element], [(Identifier, Identifier)])

main :: IO ()
main = parseFile =<< getFilePath <$> getArgs

getFilePath :: [FilePath] -> FilePath
getFilePath = fromMaybe noFile . listToMaybe
  where noFile = error "Usage: runHajure <file.cl>"

parseFile :: FilePath -> IO ()
parseFile fp = withFile fp ReadMode (printResult . parse <=< hGetContents)

parse :: Text -> ParseResult
parse = fmap transform . parseHajure
  where transform = rename . map (listify . funify)

printResult :: ParseResult -> IO ()
printResult = either print (sequenceP . (printElements *** print))

printElements :: [Element] -> IO ()
printElements = mapM_ printElement

printElement :: Element -> IO ()
printElement (Nested sexpr) = prettyPrint sexpr
printElement e              = prettyPrint e

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint = putStrLn . pshow

sequenceP :: Monad m => (m (), m ()) -> m ()
sequenceP (m, m') = m >> m'

