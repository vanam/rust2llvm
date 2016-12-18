module Falsum.Parser (module Falsum.Parser, module Falsum.Lexer, module Text.Parsec) where

import           ChangeState
import           Data.Bifunctor
import           Falsum.AST
import           Falsum.Lexer
import           Falsum.TokenTest
import           Text.Parsec                         hiding (anyToken, parse,
                                                      parseTest, satisfy)
import qualified Text.Parsec                         as P
import           Text.ParserCombinators.Parsec.Error

type Parser a = Parsec [TokenPos] ParseState a

initialState :: ParseState
initialState = ParseState []

maskState :: Parsec [TokenPos] ParseState a -> Parsec [TokenPos] () a
maskState = changeState (const ()) (const $ ParseState [])

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

addNewScope :: ParseState -> ParseState
addNewScope (ParseState scopes) = ParseState $ scopes ++ [Scope []]

removeCurrentScope :: ParseState -> ParseState
removeCurrentScope (ParseState scopes) = ParseState $ init scopes

addSymbolToScope :: Symbol -> ParseState -> ParseState
addSymbolToScope sym (ParseState scopes) =
  ParseState $ init scopes ++ [addSymbolToScope' (last scopes) sym]

addSymbolToScope' :: Scope -> Symbol -> Scope
addSymbolToScope' (Scope scope) sym = Scope (scope ++ [sym])

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
astTest p = either lexerError (P.parseTest $ maskState p) . tokenize "tokenizeParseTest"
  where
    lexerError = putStrLn . ("LEXER: " ++) . show

parser :: Parser AST
parser = fmap Token $ many anyToken

parseTest :: [TokenPos] -> IO ()
parseTest = P.parseTest $ maskState $ parser

tokenizeParseTest :: String -> IO ()
tokenizeParseTest = either lexerError parseTest . tokenize "tokenizeParseTest"
  where
    lexerError = putStrLn . ("LEXER: " ++) . show

parse :: SourceName -> [TokenPos] -> Either ParseError AST
parse = runParser parser $ ParseState []

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

literal :: Parser Token
literal = satisfy isAnyLiteral

intLiteral :: Parser Token
intLiteral = satisfy isIntLiteral

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
token2Lit (Literal x) = x

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
parseBlock =
  do
    structSymbol LBrace
    stmts <- many parseStmt
    structSymbol RBrace
    state <- getState
    modifyState addNewScope
    return stmts

parseStmt :: Parser Stmt
parseStmt = choice
              [ fmap ConstLetStmt $ parseConstLet
              , fmap VarLetStmt $ parseVarLet
              , parseLoop
              , parseWhile
              , fmap Expr $ parseExpr
              ]

parseLoop :: Parser Stmt
parseLoop = do
  keyword Falsum.Lexer.Loop
  block <- parseBlock
  return $ Falsum.AST.Loop block

parseWhile :: Parser Stmt
parseWhile = do
  keyword Falsum.Lexer.While
  cond <- parseBExpr
  whileBlock <- parseBlock
  return $ Falsum.AST.While cond whileBlock

parseExpr :: Parser Expr
parseExpr = choice
              [fmap IExpr $ parseIExpr, fmap FExpr $ parseFExpr, fmap BExpr $ parseBExpr, parseIf]

parseIf :: Parser Expr
parseIf = do
  keyword Falsum.Lexer.If
  cond <- parseBExpr
  ifBlock <- parseBlock
  elseBlock <- optionMaybe parseElse
  return $ Falsum.AST.If cond ifBlock elseBlock

parseElse :: Parser [Stmt]
parseElse = do
  keyword Else
  parseBlock

parseIExpr :: Parser IExpr
parseIExpr = choice [parseILit, parseIVar, parseINeg, parseIBinary, parseICall]

parseILit :: Parser IExpr
parseILit =
  do
    lit <- intLiteral
    return $ ILit $ getVal lit

  where
    getVal iL =
      case iL of
        (Literal (IntLit _ intVal)) -> intVal

parseIVar :: Parser IExpr
parseIVar =
  do
    symbName <- parseSymbolName
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state symbName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ IVar sym

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
    op <- satisfy isAnyOperator
    expr2 <- parseIExpr
    return $ IBinary (parseIOp $ extractOp op) expr1 expr2

  where
    extractOp (Operator o) = o

parseIOp :: Operator -> IOp
parseIOp Plus = IPlus
parseIOp Minus = IMinus
parseIOp Star = IMult
parseIOp Slash = IDiv
parseIOp Percent = IMod
parseIOp And = IAnd
parseIOp Or = IOr
parseIOp Caret = IXor

parseICall :: Parser IExpr
parseICall =
  do
    fnName <- parseSymbolName
    structSymbol LParen
    fnParams <- parseExpr `sepBy` comma
    structSymbol RParen
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state fnName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ ICall sym fnParams

parseFExpr :: Parser FExpr
parseFExpr = undefined

parseBExpr :: Parser BExpr
parseBExpr = undefined
