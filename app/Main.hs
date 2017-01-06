module Main where

import           Falsum.Codegen
import           Falsum.Parser
import           Falsum.Transform

-- import System.Directory (doesFileExist) import System.IO (hFlush, stdout)
{-
yesno :: String -> IO Bool
yesno prompt = do
  putStr $ prompt ++ " y/n: "
  hFlush stdout
  str <- getLine
  case str of
    "y" -> return True
    "n" -> return False
    _ -> do
      putStrLn "Invalid input."
      yesno prompt

main :: IO ()
main = do
  putStr "Enter a filename to start tokenization & parsing: "
  hFlush stdout
  filename <- getLine
  exists <- doesFileExist filename
  if exists
    then do
      sourceCode <- readFile filename
      parsed <- return $ tokenizeParse filename $ sourceCode
      case parsed of
        Left lexerError -> do
          putStrLn "Lexer fails:"
          putStrLn . show $ lexerError
        Right (Left parserError) -> do
          putStrLn "Parser fails:"
          putStrLn . show $ parserError
        Right (Right ast) -> do
          randomSuffix <- randomString 20
          codegen filename (transformProgram ("main_" ++ randomSuffix) ast)
    else putStrLn "The file doesn't exist."
  continue <- yesno "Continue with another file"
  if continue
    then main
    else return ()
-}
main :: IO ()
main = do
  sourceCode <- getContents
  parsed <- return $ tokenizeParse "stdin" $ sourceCode
  case parsed of
    Left lexerError -> do
      putStrLn "Lexer fails:"
      putStrLn . show $ lexerError
    Right (Left parserError) -> do
      putStrLn "Parser fails:"
      putStrLn . show $ parserError
    Right (Right ast) -> do
      rnd <- randomString 20
      putStrLn "Parser AST:"
      putStrLn . show $ ast
      transformedAst <- return $ transformProgram rnd ast
      putStrLn "" >> putStrLn "Transformed AST:"
      putStrLn . show $ transformedAst
      putStrLn "" >> putStrLn "Codegen:"
      codegen "stdin" transformedAst
