module Main where

import           Control.Monad
import           Falsum.Codegen
import           Falsum.Parser
import           Falsum.Transform
import           System.Console.GetOpt
import           System.Environment
import           System.IO

data Flag = Verbose
          | Input String
          | Output String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output on stderr"
  , Option ['i'] ["output"] (ReqArg Input "FILE") "input FILE"
  , Option ['o'] ["input"] (ReqArg Output "FILE") "output FILE"
  ]

parseOpts :: [String] -> IO (Either String ([Flag], [String]))
parseOpts argv =
  case (getOpt Permute options argv) of
    (o, n, [])   -> return $ Right (o, n)
    (_, _, errs) -> return $ Left $ concat errs ++ usageInfo header options
  where
    header = "Usage: falsum [OPTIONS]"

compile :: Handle -> Handle -> Bool -> IO ()
compile input output verbose = do
  sourceCode <- hGetContents input
  handleName <- return . show $ input
  parsed <- return $ tokenizeParse handleName $ sourceCode
  case parsed of
    Left lexerError -> do
      hPutStrLn stderr "Lexer fails:"
      hPutStrLn stderr . show $ lexerError
    Right (Left parserError) -> do
      hPutStrLn stderr "Parser fails:"
      hPutStrLn stderr . show $ parserError
    Right (Right ast) -> do
      when verbose $ do
        hPutStrLn stderr "Parser AST:"
        hPutStrLn stderr . show $ ast
      transformedAst <- return $ transformProgram ast
      when verbose $ do
        hPutStrLn stderr ""
        hPutStrLn stderr "Transformed AST:"
        hPutStrLn stderr . show $ transformedAst
      compiled <- codegen handleName transformedAst
      hPutStr output compiled

verboseFlag :: [Flag] -> Bool
verboseFlag flags = Verbose `elem` flags

inputHandle :: [Flag] -> IO Handle
inputHandle [] = return stdin
inputHandle ((Input i):_) = openFile i ReadMode
inputHandle (_:fs) = inputHandle fs

outputHandle :: [Flag] -> IO Handle
outputHandle [] = return stdout
outputHandle ((Output o):_) = openFile o WriteMode
outputHandle (_:fs) = outputHandle fs

main :: IO ()
main = do
  args <- getArgs
  maybeOpts <- parseOpts args
  case maybeOpts of
    Right (opts, _) -> do
      v <- return $ verboseFlag opts
      i <- inputHandle opts
      o <- outputHandle opts
      compile i o v
      hClose o
      hClose i
    Left err -> hPutStrLn stderr err
