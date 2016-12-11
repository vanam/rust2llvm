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
                       VarSymbol name _   -> name
                       ConstSymbol name _ -> name
                       FnSymbol name _    -> name
      maybeSym = search scope ident
  in case maybeSym of
    Nothing -> lookupSymbol (ParseState scopes) ident
    _       -> maybeSym

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

parseType :: Parser ValueType
parseType = fmap t $ choice [satisfy $ isSymbol "i32", satisfy $ isSymbol "f32"]
  where
    t (Symbol "i32") = Int
    t (Symbol "f32") = Real

symbolName :: Token -> String
symbolName (Symbol s) = s

parseSymbolName :: Parser String
parseSymbolName = fmap symbolName $ satisfy isAnySymbol

operator :: Operator -> Parser ()
operator o = (satisfy $ isOperator o) *> pure ()

keyword :: Keyword -> Parser ()
keyword k = (satisfy $ isKeyword k) *> pure ()

structSymbol :: StructureSymbol -> Parser ()
structSymbol sym = (satisfy $ isStructSymbol sym) *> pure ()

literal :: Literal -> Parser ()
literal l = (satisfy $ isLiteral l) *> pure ()

comma :: Parser ()
comma = structSymbol Comma

parseVarLet :: Parser VarLet
parseVarLet =
  do
    keyword Let
    optional $ keyword Mut
    symbName <- parseSymbolName
    structSymbol Colon
    ty <- parseType
    operator EqSign
    valueExpr <- parseExpr
    structSymbol Semicolon
    return $ VarLet (VarSymbol symbName ty) valueExpr

parseConstLet :: Parser ConstLet
parseConstLet =
  do
    keyword Const
    symbName <- parseSymbolName
    structSymbol Colon
    ty <- parseType
    operator EqSign
    valueLit <- parseLiteral
    structSymbol Semicolon
    return $ ConstLet (ConstSymbol symbName ty) valueLit

parseLiteral :: Parser Value
parseLiteral =
  do
    val <- satisfy isAnyLiteral
    return $ lit2Value $ token2Lit val

token2Lit :: Token -> Literal
-- token2Lit = Literal -- nejak se mu nechce, jak na to?
token2Lit = undefined

lit2Value :: Literal -> Value
lit2Value (IntLit _ x) = IntVal $ fromIntegral x
lit2Value (FloatLit _ (Left x)) = RealVal x
lit2Value (FloatLit _ (Right x)) = RealVal $ realToFrac x

parseFnLet :: Parser FnLet
parseFnLet =
  do
    keyword Fn
    fnName <- parseSymbolName
    structSymbol LParen
    fnParams <- parseArg `sepBy` comma
    structSymbol RParen
    fnReturnType <- optionMaybe parseReturnType
    fnBlock <- parseBlock
    return $ FnLet (FnSymbol fnName fnReturnType) fnParams fnBlock

parseArg :: Parser Symbol
parseArg =
  do
    argName <- parseSymbolName
    structSymbol Colon
    argType <- parseType
    return $ VarSymbol argName argType

parseReturnType :: Parser ValueType
parseReturnType = structSymbol RArrow *> parseType

parseBlock :: Parser [Stmt]
parseBlock = undefined

parseExpr :: Parser Expr
parseExpr = choice [parseIExpr, parseFExpr, parseBExpr]

parseIExpr :: Parser IExpr
parseIExpr = choice [parseILit, parseIVar, parseINeg, parseIBinary, parseICall]

parseILit :: Parser IExpr
parseILit =
  do
    intLit <- literal IntLit
    return $ ILit (getVal intLit)

  where
    getVal =
      case intLit of
        (IntLit _ intVal) -> intVal

-- getValOfIntLit :: Literal -> Integer getValOfIntLit (IntLit _ intVal) = intVal
parseIVar :: Parser IExpr
parseIVar =
  do
    symbName <- parseSymbolName
    structSymbol Semicolon
    let ls = lookupSymbol {-get parser state-} symbName
    return $ IVar (Just ls) -- checknout jestli je to opravdu VarSymbol

parseINeg :: Parser IExpr
parseINeg =
  do
    operator Minus
    expr <- parseIExpr
    return $ INeg expr

parseIBinary :: Parser IExpr
parseIBinary =
  do
    expr1 <- parseIExpr
    operator <- satisfy isAnyOperator
    expr2 <- parseIExpr
    return $ IBinary (parseIOp operator) expr1 expr2

parseIOp :: Operator -> IOp
parseIOp Plus = IPlus
parseIOp Minus = IMinus
parseIOp Star = IMult
parseIOp Slash = IDiv
parseIOp Percent = IMod
parseIOp And = IAnd
parseIOp Or = IOr
parseIOp Caret = IXor

--Symbol "bar",StructSym LParen,Symbol "moje_promenna",StructSym RParen,StructSym Semicolon,
parseICall :: Parser IExpr
parseICall =
  do
    fnName <- parseSymbolName
    structSymbol LParen
    fnParams <- parseArg `sepBy` comma
    structSymbol RParen
    structSymbol Semicolon
    let ls = lookupSymbol {-get parser state-} fnName
    return $ ICall (Just ls) fnParams

parseFExpr :: Parser FExpr
parseFExpr = undefined

parseBExpr :: Parser BExpr
parseBExpr = undefined
