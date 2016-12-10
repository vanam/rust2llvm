module Falsum.Parser (module Falsum.Parser, module Falsum.Lexer, module Text.Parsec) where

import           Data.Bifunctor
import           Falsum.AST
import           Falsum.Lexer
import           Falsum.TokenTest
import           Text.Parsec                         hiding (anyToken, parse,
                                                      parseTest, satisfy)
import qualified Text.Parsec                         as P
import           Text.ParserCombinators.Parsec.Error

type Parser a = Parsec [TokenPos] () a

data AST = Token [Token]
  deriving (Show, Eq)

lookupSymbol :: ParseState -> String -> Maybe Symbol
lookupSymbol (ParseState []) _ = Nothing
lookupSymbol (ParseState (scope:scopes)) ident =
  let search (Scope []) _ = Nothing
      search (Scope (sym:syms)) i
        | symName == i = Just sym
        | otherwise = search (Scope syms) i
        where
          symName =
                     case sym of
                       VarSymbol name _     -> name
                       ConstSymbol name _ _ -> name
                       FnSymbol name _ _    -> name
      maybeSym = search scope ident
  in case maybeSym of
    Nothing -> lookupSymbol (ParseState scopes) ident
    _       -> maybeSym

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos):_) = pos
advance pos _ []         = pos

satisfy' :: (TokenPos -> Bool) -> Parser Token
satisfy' f = tokenPrim show advance
               (\c -> if f c
                        then Just (fst c)
                        else Nothing)

satisfy :: (Token -> Bool) -> Parser Token
satisfy test = satisfy' $ test . fst

anyToken :: Parser Token
anyToken = choice . map satisfy $ map isKeyword [Let, Mut] ++ [ isAnyCoupledDoc
                                  , isAnyCoupledAttribute
                                  , isAnySymbol
                                  , isAnyLiteral
                                  , isAnyLifeTime
                                  , isAnyKeyword
                                  , isAnyStructSym
                                  , isAnyOperator
                                  , isAnyCoupledDoc
                                  , isAnyCoupledAttribute
                                  ]

astTest :: Show a => Parser a -> String -> IO ()
astTest p = either lexerError (P.parseTest p) . tokenize "tokenizeParseTest"
  where
    lexerError = putStrLn . ("LEXER: " ++) . show

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

letStmt :: Parser VarLet
letStmt =
  do
    satisfy $ isKeyword Let
    optional . satisfy $ isKeyword Mut
    symb <- satisfy $ isAnySymbol
    satisfy $ isStructSymbol Colon
    symbType <- choice [satisfy $ isSymbol "i32", satisfy $ isSymbol "f32"]
    satisfy $ isOperator EqSign
    valueExpr <- parseIExpr
    satisfy $ isStructSymbol Semicolon
    return $ VarLet (VarSymbol (symbolName symb) (t symbType)) (IExpr valueExpr)
  where symbolName (Symbol s) = s
        t (Symbol "i32") = Int
        t (Symbol "f32") = Real


parseIExpr :: Parser IExpr
parseIExpr = undefined -- @TODO
