
module Main where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad ((<=<), void)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import System.Environment
import System.IO hiding (hGetContents)

import Hajure.AST
import Hajure.Data
import Hajure.Parsing
import ParsecImports (ParseError)

type ParseResult = Either ParseError ([Element], Mappings)

main :: IO ()
main = parseFile =<< getFilePath <$> getArgs

getFilePath :: [FilePath] -> FilePath
getFilePath = fromMaybe noFile . listToMaybe
  where noFile = error "Usage: runHajure <file.cl>"

parseFile :: FilePath -> IO ()
parseFile fp = withFile fp ReadMode (printResult . parse fp <=< hGetContents)

parse :: FilePath -> Text -> ParseResult
parse fp = fmap transform . parseHajure fp
  where transform = rename . map (listify . funify)

printResult :: ParseResult -> IO ()
printResult = either print (sequenceP . (printElements *** printMappings))

printElements :: [Element] -> IO ()
printElements = mapM_ printElement

printElement :: Element -> IO ()
printElement (Nested sexpr) = prettyPrint sexpr
printElement e              = prettyPrint e

printMappings :: Mappings -> IO ()
printMappings = (putStrLn "\n\nName Mappings:\n" >>) . prettyPrint

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint = putStrLn . pshow

sequenceP :: Applicative m => (m a, m b) -> m ()
sequenceP = void . uncurry (*>)

