module Main where

import           Falsum.Codegen
import           Falsum.Parser
import           Falsum.Transform
import           System.IO

main :: IO ()
main = do
  sourceCode <- getContents
  parsed <- return $ tokenizeParse "stdin" $ sourceCode
  case parsed of
    Left lexerError -> do
      hPutStrLn stderr "Lexer fails:"
      hPutStrLn stderr . show $ lexerError
    Right (Left parserError) -> do
      hPutStrLn stderr "Parser fails:"
      hPutStrLn stderr . show $ parserError
    Right (Right ast) -> do
      hPutStrLn stderr "Parser AST:"
      hPutStrLn stderr . show $ ast
      transformedAst <- return $ transformProgram ast
      hPutStrLn stderr ""
      hPutStrLn stderr "Transformed AST:"
      hPutStrLn stderr . show $ transformedAst
      codegen "stdin" transformedAst
