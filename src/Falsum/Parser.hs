module Falsum.Parser (module Falsum.Parser, module Falsum.Lexer, module Text.Parsec) where

import           ChangeState
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.List
import           Falsum.AST
import           Falsum.Lexer
import           Falsum.TokenTest
import           Text.Parsec                         hiding (anyToken, parse,
                                                      parseTest, satisfy)
import qualified Text.Parsec                         as P
import qualified Text.Parsec.Expr                    as E
import           Text.ParserCombinators.Parsec.Error

type Parser a = Parsec [TokenPos] ParseState a

initialState :: ParseState
initialState = ParseState [Scope []] Nothing

maskState :: Parsec [TokenPos] ParseState a -> Parsec [TokenPos] () a
maskState = changeState (const ()) (const initialState)

lookupSymbol :: ParseState -> String -> Maybe Symbol
lookupSymbol (ParseState [] _) _ = Nothing
lookupSymbol (ParseState (scope:scopes) returnType) ident =
  let search (Scope []) _ = Nothing
      search (Scope (sym:syms)) i
        | symName == i = Just sym
        | otherwise = search (Scope syms) i
        where
          symName =
                     case sym of
                       GlobalVarSymbol name _ -> name
                       VarSymbol name _       -> name
                       ConstSymbol name _     -> name
                       FnSymbol name _        -> name
      maybeSym = search scope ident
  in case maybeSym of
    Nothing -> lookupSymbol (ParseState scopes returnType) ident
    _       -> maybeSym

safeLookupSymbol :: String -> String -> Parser Symbol
safeLookupSymbol searchSymbol failMsg = maybe (unexpected $ failMsg ++ show searchSymbol) pure =<< flip
                                                                                                     lookupSymbol
                                                                                                     searchSymbol <$> getState

addNewScope :: ParseState -> ParseState
addNewScope (ParseState scopes returnType) = ParseState ([Scope []] ++ scopes) returnType

removeCurrentScope :: ParseState -> ParseState
removeCurrentScope (ParseState scopes returnType) = ParseState (tail scopes) returnType

addSymbolToScope :: Symbol -> ParseState -> ParseState
addSymbolToScope sym (ParseState (scopesHead:scopesTail) returnType) =
  ParseState (addSymbolToScope' scopesHead sym : scopesTail) returnType
addSymbolToScope _ st = st

addSymbolToScope' :: Scope -> Symbol -> Scope
addSymbolToScope' (Scope scope) sym = Scope (sym : scope)

setReturnTypeOfScope :: ParseState -> Maybe ValueType -> ParseState
setReturnTypeOfScope (ParseState scopes _) valueType = ParseState scopes valueType

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
                          [ TopFnLet <$> parseFnLet
                          , TopConstLet <$> parseConstLet
                          , TopVarLet <$> parseVarLet
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
parse = runParser parser initialState

tokenizeParse :: SourceName -> String -> Either ParseError (Either ParseError Program)
tokenizeParse sn = bimap lexerError (parse sn) . tokenize sn
  where
    lexerError = addErrorMessage (Message "LEXER complaints")

parseType :: Parser ValueType
parseType = t <$> choice [satisfy $ isSymbol "i32", satisfy $ isSymbol "f32"]
  where
    t (Symbol "i32") = Int
    t (Symbol "f32") = Real

symbolName :: Token -> String
symbolName (Symbol s) = s

parseSymbolName :: Parser String
parseSymbolName = symbolName <$> satisfy isAnySymbol

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

inParens :: Parser a -> Parser a
inParens = between (structSymbol LParen) (structSymbol RParen)

inBrackets :: Parser a -> Parser a
inBrackets = between (structSymbol LBrack) (structSymbol RBrack)

inBraces :: Parser a -> Parser a
inBraces = between (structSymbol LBrace) (structSymbol RBrace)

binary :: E.Assoc -> Operator -> (a -> a -> a) -> E.Operator [TokenPos] ParseState Identity a
binary assoc op con = flip E.Infix assoc $ operator op *> pure con

binaryl :: Operator -> (a -> a -> a) -> E.Operator [TokenPos] ParseState Identity a
binaryl = binary E.AssocLeft

binaryr :: Operator -> (a -> a -> a) -> E.Operator [TokenPos] ParseState Identity a
binaryr = binary E.AssocRight

binaryn :: Operator -> (a -> a -> a) -> E.Operator [TokenPos] ParseState Identity a
binaryn = binary E.AssocNone

prefix :: Operator -> (a -> a) -> E.Operator [TokenPos] ParseState Identity a
prefix op con = E.Prefix $ operator op *> pure con

postfix :: Operator -> (a -> a) -> E.Operator [TokenPos] ParseState Identity a
postfix op con = E.Postfix $ operator op *> pure con

comma :: Parser ()
comma = structSymbol Comma

currentScope :: ParseState -> Int
currentScope (ParseState scopes _) = length scopes

forgeSymbol :: String -> ValueType -> Parser Symbol
forgeSymbol symbName ty =
  do
    state <- getState
    return $ (if currentScope state == 1
                then GlobalVarSymbol
                else VarSymbol)
               symbName
               ty

parseVarLet :: Parser VarLet
parseVarLet =
  do
    symbName <- parseVarSymbolName
    structSymbol Colon
    ty <- parseType
    operator EqSign
    valueExpr <- parseExpr
    structSymbol Semicolon
    state <- getState
    forgedSymbol <- forgeSymbol symbName ty
    putState $ addSymbolToScope forgedSymbol state
    return $ VarLet forgedSymbol valueExpr

parseBinVarLet :: Parser VarLet
parseBinVarLet =
  do
    symbName <- parseVarSymbolName
    operator EqSign
    valueExpr <- parseBExpr
    structSymbol Semicolon
    state <- getState
    forgedSymbol <- forgeSymbol symbName Bool
    putState $ addSymbolToScope forgedSymbol state
    return $ VarLet forgedSymbol $ getExpr valueExpr

  where
    getExpr :: BExpr -> Expr
    getExpr = BExpr

parseVarSymbolName :: Parser String
parseVarSymbolName =
  do
    choice [keyword Let, keyword Static]
    optional $ keyword Mut
    parseSymbolName

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
    state <- getState
    putState $ addSymbolToScope (ConstSymbol symbName ty) state
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
    state <- getState
    if currentScope state > 1
      then unexpected "Defining function out of root scope"
      else return ()
    fnName <- parseSymbolName
    fnParams <- inParens $ parseArg `sepBy` comma
    fnReturnType <- optionMaybe parseReturnType
    putState $ setReturnTypeOfScope state fnReturnType
    modifyState addNewScope
    addParamsToScope fnParams
    fnBlock <- parseBlock
    modifyState removeCurrentScope
    putState $ addSymbolToScope (FnSymbol fnName fnReturnType) state
    return $ FnLet (FnSymbol fnName fnReturnType) fnParams fnBlock

  where
    addParamsToScope = mapM_ (modifyState . addSymbolToScope)

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
parseBlock = inBraces $ modifyState addNewScope *> many parseStmt <* modifyState removeCurrentScope

parseStmt :: Parser Stmt
parseStmt = choice
              [ ConstLetStmt <$> parseConstLet
              , VarLetStmt <$> parseVarLet
              , VarLetStmt <$> parseBinVarLet
              , parseLoop
              , parseWhile
              , parseReturn
              , parseVCall
              , (Expr <$> parseExpr) <* structSymbol Semicolon
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

parseReturn :: Parser Stmt
parseReturn = do
  keyword Falsum.Lexer.Return
  r <- rType <$> getState
  case r of
    Nothing -> do
      structSymbol Semicolon <|> mismatch r
      return $ Falsum.AST.Return Nothing
    Just Int -> do
      expr <- parseIExpr <|> mismatch r
      structSymbol Semicolon
      return $ Falsum.AST.Return (Just (IExpr expr))
    Just Real -> do
      expr <- parseFExpr <|> mismatch r
      structSymbol Semicolon
      return $ Falsum.AST.Return (Just (FExpr expr))
    Just Bool -> do
      expr <- parseBExpr <|> mismatch r
      structSymbol Semicolon
      return $ Falsum.AST.Return (Just (BExpr expr))

  where
    rType (ParseState _ r) = r
    mismatch r = unexpected $ "Return type does not match, expected: " ++ show r

parseExpr :: Parser Expr
parseExpr = choice [IExpr <$> parseIExpr, FExpr <$> parseFExpr, BExpr <$> parseBExpr, parseIf]

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

parseVCall :: Parser Stmt
parseVCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    structSymbol Semicolon
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    return $ VCall fnSym fnParams

parseITerm :: Parser IExpr
parseITerm = choice [inParens parseIExpr, parseILit, try parseICall, parseIVar]
             <?> "simple int expression" -- TODO add parseIIf -- if expression with integer result
                                         -- (with required else branch?)

iBinaryTable :: [[E.Operator [TokenPos] ParseState Identity IExpr]]
iBinaryTable = [ [prefix Minus INeg]
               , [ binaryl Star (IBinary IMult)
                 , binaryl Slash (IBinary IDiv)
                 , binaryl Percent (IBinary IMod)
                 ]
               , [binaryl Plus (IBinary IPlus), binaryl Minus (IBinary IMinus)]
               , [binaryl And (IBinary IAnd)]
               , [binaryl Caret (IBinary IXor)]
               , [binaryl Or (IBinary IOr)]
               ] -- TODO optionally shift operators as postfix operators?

parseIExpr :: Parser IExpr
parseIExpr = E.buildExpressionParser iBinaryTable parseITerm <?> "complex int expression"

parseILit :: Parser IExpr
parseILit = ILit . getVal <$> intLiteral
  where
    getVal (Literal (IntLit _ intVal)) = intVal

parseIVar :: Parser IExpr
parseIVar =
  do
    symbName <- parseSymbolName
    sym <- safeLookupSymbol symbName "Missing symbol: "
    return $ IVar sym

parseICall :: Parser IExpr
parseICall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    return $ ICall fnSym fnParams

parseFTerm :: Parser FExpr
parseFTerm = choice [inParens parseFExpr, parseFLit, parseFVar, parseFCall]
             <?> "simple float expression" -- TODO add parseFIf -- if expression with real result
                                           -- (with required else branch?)

fBinaryTable :: [[E.Operator [TokenPos] ParseState Identity FExpr]]
fBinaryTable = [ [prefix Minus FNeg]
               , [binaryl Star (FBinary FMult), binaryl Slash (FBinary FDiv)]
               , [binaryl Plus (FBinary FPlus), binaryl Minus (FBinary FMinus)]
               ]

parseFExpr :: Parser FExpr
parseFExpr = E.buildExpressionParser fBinaryTable parseFTerm
             <?> "complex float expression"

parseFLit :: Parser FExpr
parseFLit = FLit . getVal <$> floatLiteral
  where
    getVal (Literal (FloatLit Nothing (Left floatVal))) = floatVal

parseFVar :: Parser FExpr
parseFVar =
  do
    symbName <- parseSymbolName
    sym <- safeLookupSymbol symbName "Missing symbol: "
    return $ FVar sym

parseFCall :: Parser FExpr
parseFCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    structSymbol Semicolon
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    return $ FCall fnSym fnParams

parseBTerm :: Parser BExpr
parseBTerm = choice
               [inParens parseBExpr, parseTrue, parseFalse, parseBVar, parseBCall, parseRelation]
             <?> "simple bool expression" -- TODO add parseBIf -- if expression with boolean result
                                          -- (with required else branch?)

bBinaryTable :: [[E.Operator [TokenPos] ParseState Identity BExpr]]
bBinaryTable = [ [prefix Not BNot]
               , [binaryl DoubleAnd (BBinary BAnd)]
               , [binaryl DoubleOr (BBinary BOr)]
               ]

parseBExpr :: Parser BExpr
parseBExpr = E.buildExpressionParser bBinaryTable parseBTerm
             <?> "complex bool expression"

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
    sym <- safeLookupSymbol symbName "Missing symbol: "
    return $ BVar sym

parseBCall :: Parser BExpr
parseBCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    structSymbol Semicolon
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    return $ BCall fnSym fnParams

parseRelation :: Parser BExpr
parseRelation = parserZero -- TODO choice [parseBRBinary, parseIRBinary, parseFRBinary]

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

parseROp :: Operator -> ROp
parseROp Falsum.Lexer.Less = Falsum.AST.Less
parseROp Falsum.Lexer.Leq = Falsum.AST.LessEqual
parseROp Falsum.Lexer.Greater = Falsum.AST.Greater
parseROp Falsum.Lexer.Geq = Falsum.AST.GreaterEqual
parseROp Falsum.Lexer.DoubleEq = Falsum.AST.Equal
parseROp Falsum.Lexer.Neq = Falsum.AST.NotEqual
