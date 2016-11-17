module Falsum.Parser (module Falsum.Parser, module Falsum.Lexer, module Text.Parsec) where

import           Data.Bifunctor
import           Falsum.Lexer
import           Falsum.TokenTest
import           Text.Parsec                         hiding (anyToken, parse,
                                                      parseTest, satisfy)
import qualified Text.Parsec                         as P
import           Text.ParserCombinators.Parsec.Error

type Parser a = Parsec [TokenPos] () a

data AST = Token [Token]
  deriving (Show, Eq)

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos):_) = pos
advance pos _ [] = pos

satisfy' :: (TokenPos -> Bool) -> Parser Token
satisfy' f = tokenPrim show advance
               (\c -> if f c
                        then Just (fst c)
                        else Nothing)

satisfy :: (Token -> Bool) -> Parser Token
satisfy test = satisfy' $ test . fst

anyToken :: Parser Token
anyToken = choice . map satisfy $ [ isSymbol
                                  , isLiteral
                                  , isLifeTime
                                  , isKeyword
                                  , isStructSym
                                  , isOperator
                                  , isCoupledDoc
                                  , isCoupledAttribute
                                  ]

parser :: Parser AST
parser = fmap Token $ many anyToken

parseTest :: [TokenPos] -> IO ()
parseTest = P.parseTest parser

tokenizeParseTest :: String -> IO ()
tokenizeParseTest = either lexerError parseTest . tokenize "tokenizeParseTest"
  where
    lexerError = putStrLn . ("LEXER: " ++) . show

parse :: SourceName -> [TokenPos] -> Either ParseError AST
parse = runParser parser ()

tokenizeParse :: SourceName -> String -> Either ParseError (Either ParseError AST)
tokenizeParse sn = bimap lexerError (parse sn) . tokenize sn
  where
    lexerError = addErrorMessage (Message "LEXER complaints")
