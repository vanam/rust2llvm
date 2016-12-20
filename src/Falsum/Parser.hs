module Falsum.Parser (module Falsum.Parser, module Falsum.Lexer, module Text.Parsec) where

import           ChangeState
import           Data.Bifunctor
import           Data.List
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

{-
data AST = Token [Token]
  deriving (Show, Eq)
-}
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

isMain :: FnLet -> Bool
isMain (FnLet (FnSymbol name Nothing) [] _) = name == "main"
isMain _ = False

parseTopLevel :: Parser [TopLevel]
parseTopLevel = many1 $ choice
                          [ fmap TopFnLet $ parseFnLet
                          , fmap TopConstLet $ parseConstLet
                          , fmap TopVarLet $ parseVarLet
                          ]

parser :: Parser Program
parser = do
  tops <- fmap split parseTopLevel
  case find isMain (funs tops) of
    Nothing   -> unexpected "Missing the main function with the right signature"
    Just main -> return $ Program (consts tops) (vars tops) (filter (not . isMain) (funs tops)) main

  where
    split' (TopFnLet f) (cs, vs, fs) = (cs, vs, f : fs)
    split' (TopConstLet c) (cs, vs, fs) = (c : cs, vs, fs)
    split' (TopVarLet v) (cs, vs, fs) = (cs, v : vs, fs)
    split = foldr split' ([], [], [])
    consts (cs, _, _) = cs
    vars (_, vs, _) = vs
    funs (_, _, fs) = fs

parseTest :: [TokenPos] -> IO ()
parseTest = P.parseTest $ maskState $ parser

tokenizeParseTest :: String -> IO ()
tokenizeParseTest = either lexerError parseTest . tokenize "tokenizeParseTest"
  where
    lexerError = putStrLn . ("LEXER: " ++) . show

parse :: SourceName -> [TokenPos] -> Either ParseError Program
parse = runParser parser $ ParseState []

tokenizeParse :: SourceName -> String -> Either ParseError (Either ParseError Program)
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

floatLiteral :: Parser Token
floatLiteral = satisfy isFloatLiteral

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

parseBinVarLet :: Parser VarLet
parseBinVarLet =
  do
    keyword Let
    optional $ keyword Mut
    symbName <- parseSymbolName
    operator EqSign
    valueExpr <- parseBExpr
    structSymbol Semicolon
    return $ VarLet (VarSymbol symbName Bool) $ getExpr valueExpr

  where
    getExpr :: BExpr -> Expr
    getExpr = BExpr

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
    state <- getState
    if curretScope state > 1
      then unexpected "Defining function out of root scope"
      else return ()
    return $ FnLet (FnSymbol fnName fnReturnType) fnParams fnBlock

  where
    curretScope (ParseState scopes) = length scopes

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
    modifyState addNewScope
    return stmts

parseStmt :: Parser Stmt
parseStmt = choice
              [ fmap ConstLetStmt $ parseConstLet
              , fmap VarLetStmt $ parseVarLet
              , fmap VarLetStmt $ parseBinVarLet
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
parseFExpr = choice [parseFLit, parseFVar, parseFNeg, parseFBinary, parseFCall]

parseFLit :: Parser FExpr
parseFLit =
  do
    lit <- floatLiteral
    return $ FLit $ getVal lit

  where
    getVal fL =
      case fL of
        (Literal (FloatLit Nothing (Left floatVal))) -> floatVal

parseFVar :: Parser FExpr
parseFVar =
  do
    symbName <- parseSymbolName
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state symbName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ FVar sym

parseFNeg :: Parser FExpr
parseFNeg =
  do
    operator Minus
    expr <- parseFExpr
    return $ FNeg expr

parseFBinary :: Parser FExpr
parseFBinary =
  do
    expr1 <- parseFExpr
    op <- satisfy isAnyOperator
    expr2 <- parseFExpr
    return $ FBinary (parseFOp $ extractOp op) expr1 expr2

  where
    extractOp (Operator o) = o

parseFOp :: Operator -> FOp
parseFOp Plus = FPlus
parseFOp Minus = FMinus
parseFOp Star = FMult
parseFOp Slash = FDiv

parseFCall :: Parser FExpr
parseFCall =
  do
    fnName <- parseSymbolName
    structSymbol LParen
    fnParams <- parseExpr `sepBy` comma
    structSymbol RParen
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state fnName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ FCall sym fnParams

parseBExpr :: Parser BExpr
parseBExpr = choice
               [ parseTrue
               , parseFalse
               , parseBVar
               , parseBNot
               , parseBBinary
               , parseIRBinary
               , parseFRBinary
               , parseBCall
               ]

parseTrue :: Parser BExpr
parseTrue =
  do
    keyword TrueLit
    return $ BLit True

parseFalse :: Parser BExpr
parseFalse =
  do
    keyword FalseLit
    return $ BLit False

parseBVar :: Parser BExpr
parseBVar =
  do
    symbName <- parseSymbolName
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state symbName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ BVar sym

parseBNot :: Parser BExpr
parseBNot =
  do
    operator Falsum.Lexer.Not
    expr <- parseBExpr
    return $ Falsum.AST.Not expr

parseBBinary :: Parser BExpr
parseBBinary =
  do
    expr1 <- parseBExpr
    op <- satisfy isAnyOperator
    expr2 <- parseBExpr
    return $ BBinary (parseBOp $ extractOp op) expr1 expr2

  where
    extractOp (Operator o) = o

parseIRBinary :: Parser BExpr
parseIRBinary =
  do
    expr1 <- parseIExpr
    op <- satisfy isAnyOperator
    expr2 <- parseIExpr
    return $ IRBinary (parseROp $ extractOp op) expr1 expr2

  where
    extractOp (Operator o) = o

parseFRBinary :: Parser BExpr
parseFRBinary =
  do
    expr1 <- parseFExpr
    op <- satisfy isAnyOperator
    expr2 <- parseFExpr
    return $ FRBinary (parseROp $ extractOp op) expr1 expr2

  where
    extractOp (Operator o) = o

parseBOp :: Operator -> BOp
parseBOp DoubleAnd = BAnd
parseBOp DoubleOr = BOr
parseBOp Caret = BXor

parseROp :: Operator -> ROp
parseROp Falsum.Lexer.Less = Falsum.AST.Less
parseROp Falsum.Lexer.Leq = Falsum.AST.LessEqual
parseROp Falsum.Lexer.Greater = Falsum.AST.Greater
parseROp Falsum.Lexer.Geq = Falsum.AST.GreaterEqual
parseROp Falsum.Lexer.DoubleEq = Falsum.AST.Equal
parseROp Falsum.Lexer.Neq = Falsum.AST.NotEqual

parseBCall :: Parser BExpr
parseBCall =
  do
    fnName <- parseSymbolName
    structSymbol LParen
    fnParams <- parseExpr `sepBy` comma
    structSymbol RParen
    structSymbol Semicolon
    state <- getState
    case lookupSymbol state fnName of
      Nothing  -> unexpected "Missing symbol"
      Just sym -> return $ BCall sym fnParams
