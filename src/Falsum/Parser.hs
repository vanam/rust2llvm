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

import           Debug.Trace

println :: Show a => a -> ParsecT [TokenPos] u Identity ()
println msg = trace (show msg) $ return ()

seeNext :: Int -> ParsecT [TokenPos] u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

type Parser a = Parsec [TokenPos] ParseState a

initialState :: ParseState
initialState = ParseState [Scope [VariadicFnSymbol "printf" [String] Nothing]] Nothing

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
                       GlobalVarSymbol name _    -> name
                       VarSymbol name _          -> name
                       ConstSymbol name _        -> name
                       FnSymbol name _ _         -> name
                       VariadicFnSymbol name _ _ -> name
      maybeSym = search scope ident
  in case maybeSym of
    Nothing -> lookupSymbol (ParseState scopes returnType) ident
    _       -> maybeSym

safeLookupSymbol :: String -> String -> Parser Symbol
safeLookupSymbol searchSymbol failMsg = maybe (unexpected $ failMsg ++ show searchSymbol) pure =<< flip
                                                                                                     lookupSymbol
                                                                                                     searchSymbol <$> getState

checkSymbolNotExists :: ParseState -> String -> Parser ()
checkSymbolNotExists state sym = do
  checkExists $ lookupSymbol state sym
  return ()

  where
    checkExists Nothing = return ()
    checkExists (Just x) = unexpected $ "Symbol " ++ show x ++ " allready exist"

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
isMain (FnLet (FnSymbol name [] Nothing) [] _) = name == "main"
isMain _ = False

parseTopLevel :: Parser [TopLevel]
parseTopLevel = many1 $ choice
                          [ TopFnLet <$> parseFnLet
                          , TopConstLet <$> parseConstLet
                          , TopVarLet <$> parseIVarLet
                          , TopVarLet <$> parseFVarLet
                          , TopVarLet <$> parseBinVarLet
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
parseType = t <$> choice
                    [satisfy $ isSymbol "i32", satisfy $ isSymbol "f32", satisfy $ isSymbol "bool"]
  where
    t (Symbol "i32") = Int
    t (Symbol "f32") = Real
    t (Symbol "bool") = Bool

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

checkVarType :: Symbol -> Expr -> Parser ()
checkVarType (VarSymbol _ Int) (IExpr _) = return () -- TODO can be written better
checkVarType (ConstSymbol _ Int) (IExpr _) = return () -- eg. with some sort of data alias
checkVarType (GlobalVarSymbol _ Int) (IExpr _) = return ()
checkVarType (VarSymbol _ Real) (FExpr _) = return ()
checkVarType (ConstSymbol _ Real) (FExpr _) = return ()
checkVarType (GlobalVarSymbol _ Real) (FExpr _) = return ()
checkVarType (VarSymbol _ Bool) (BExpr _) = return ()
checkVarType (ConstSymbol _ Bool) (BExpr _) = return ()
checkVarType (GlobalVarSymbol _ Bool) (BExpr _) = return ()
checkVarType (VarSymbol _ String) (SExpr _) = return ()
checkVarType _ _ = unexpected "Var type does not match"

checkFnCallParams :: Symbol -> [Expr] -> Parser ()
checkFnCallParams (VariadicFnSymbol _ [] _) [] = return ()
checkFnCallParams (FnSymbol _ [] _) [] = return ()
checkFnCallParams sym params
  | (FnSymbol _ types _) <- sym = do
      if length types /= length params
        then unexpected "Arguments count does not match"
        else return ()
      last $ zipWith check types params
      return ()
  | (VariadicFnSymbol _ types _) <- sym = do
      last $ zipWith check types params
      return ()
  where
    check Int (IExpr _) = return ()
    check Bool (BExpr _) = return ()
    check Real (FExpr _) = return ()
    check String (SExpr _) = return ()
    check a b = unexpected $ "Function argument does not match " ++ show a ++ " " ++ show b

parseIVarLet :: Parser VarLet
parseIVarLet =
  do
    symbName <- parseVarSymbolName
    state <- getState
    checkSymbolNotExists state symbName
    structSymbol Colon
    satisfy $ isSymbol "i32"
    operator EqSign
    valueExpr <- parseIExpr
    case valueExpr of
      IIf{} -> return ()
      _     -> structSymbol Semicolon
    forgedSymbol <- forgeSymbol symbName Int
    putState $ addSymbolToScope forgedSymbol state
    return $ VarLet forgedSymbol (IExpr (IAssign (LValue forgedSymbol) valueExpr))

parseFVarLet :: Parser VarLet
parseFVarLet =
  do
    symbName <- parseVarSymbolName
    state <- getState
    checkSymbolNotExists state symbName
    structSymbol Colon
    satisfy $ isSymbol "f32"
    operator EqSign
    valueExpr <- parseFExpr
    structSymbol Semicolon
    forgedSymbol <- forgeSymbol symbName Real
    putState $ addSymbolToScope forgedSymbol state
    return $ VarLet forgedSymbol (FExpr (FAssign (LValue forgedSymbol) valueExpr))

parseBinVarLet :: Parser VarLet
parseBinVarLet =
  do
    symbName <- parseVarSymbolName
    state <- getState
    checkSymbolNotExists state symbName
    optional parseBoolType
    operator EqSign
    valueExpr <- parseBExpr
    structSymbol Semicolon
    forgedSymbol <- forgeSymbol symbName Bool
    putState $ addSymbolToScope forgedSymbol state
    return $ VarLet forgedSymbol (BExpr (BAssign (LValue forgedSymbol) valueExpr))

  where
    parseBoolType = do
      structSymbol Colon
      satisfy $ isSymbol "bool"
      return ()

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
    state <- getState
    checkSymbolNotExists state symbName
    structSymbol Colon
    ty <- parseType
    operator EqSign
    valueLit <- parseLiteral
    structSymbol Semicolon
    putState $ addSymbolToScope (ConstSymbol symbName ty) state
    return $ ConstLet (ConstSymbol symbName ty) valueLit

parseLiteral :: Parser Value
parseLiteral =
  do
    val <- satisfy isAnyLiteral
    isSupportedLiteral $ token2Lit val
    return $ lit2Value $ token2Lit val

  where
    isSupportedLiteral (IntLit _ _) = return ()
    isSupportedLiteral (FloatLit _ _) = return ()
    isSupportedLiteral _ = unexpected "Expecting Int or Float Literal"

token2Lit :: Token -> Literal
token2Lit (Literal x) = x

lit2Value :: Literal -> Value
lit2Value (IntLit _ x) = IntVal $ fromIntegral x
lit2Value (FloatLit _ (Left x)) = RealVal x
lit2Value (FloatLit _ (Right x)) = RealVal $ realToFrac x
lit2Value _ = undefined -- shlould not happend function parseLiteral allready check for supported
                        -- literals

-- lit2Value (ByteString s) = StringVal s lit2Value (UnicodeString s) = StringVal s
parseFnLet :: Parser FnLet
parseFnLet =
  do
    keyword Fn
    state <- getState
    if currentScope state > 1
      then unexpected "Defining function out of root scope"
      else return ()
    fnName <- parseSymbolName
    checkSymbolNotExists state fnName
    fnParams <- inParens $ parseArg `sepBy` comma
    fnReturnType <- optionMaybe parseReturnType
    putState $ addSymbolToScope (FnSymbol fnName (map getType fnParams) fnReturnType) state
    modifyState $ flip setReturnTypeOfScope fnReturnType
    modifyState addNewScope
    addParamsToScope fnParams
    fnBlock <- parseBlock
    checkBlockType fnReturnType fnBlock
    modifyState removeCurrentScope
    return $ FnLet (FnSymbol fnName (map getType fnParams) fnReturnType) fnParams fnBlock

  where
    addParamsToScope = mapM_ (modifyState . addSymbolToScope)
    getType (VarSymbol _ valType) = valType

    checkBlockType :: Maybe ValueType -> [Stmt] -> Parser ()
    checkBlockType Nothing _ = return ()
    checkBlockType (Just blockType) statements = do
      lastStatement <- getLastStmt statements
      checkStatementType blockType lastStatement

    checkExprType :: ValueType -> Expr -> Parser ()
    checkExprType Int (IExpr _) = return ()
    checkExprType Real (FExpr _) = return ()
    checkExprType Bool (BExpr _) = return ()
    checkExprType _ _ = unexpected "Expected type does not match"

    getLastStmt [] = unexpected "Missing return or implicit return expression"
    getLastStmt statements = pure $ last statements

    checkStatementType :: ValueType -> Stmt -> Parser ()
    checkStatementType t (Expr expr) = checkExprType t expr
    checkStatementType expectedType (Falsum.AST.Return (Just expr)) = checkExprType expectedType
                                                                        expr
    checkStatementType _ _ = unexpected "Missing return or implicit return expression"

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

parseAnyStmt :: Parser Stmt
parseAnyStmt = choice
                 [ ConstLetStmt <$> parseConstLet
                 , VarLetStmt <$> try parseIVarLet
                 , VarLetStmt <$> try parseFVarLet
                 , VarLetStmt <$> try parseBinVarLet
                 , parseLoop
                 , parseWhile
                 , parseReturn
                 , try parseVCall
                 ]

parseStmt :: Parser Stmt
parseStmt = choice [parseAnyStmt, Expr <$> (IExpr <$> try parseIIf) -- no semicolon
                                  , parseIf, (Expr <$> parseExpr) <* structSymbol Semicolon]

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
parseExpr = choice [BExpr <$> try parseBExpr, IExpr <$> try parseIExpr, FExpr <$> try parseFExpr]

parseIf :: Parser Stmt
parseIf = do
  keyword Falsum.Lexer.If
  cond <- parseBExpr
  ifBlock <- parseBlock
  elseBlock <- optionMaybe parseElse
  return $ Falsum.AST.If cond ifBlock elseBlock

  where
    parseElse = do
      keyword Else
      choice [parseIfAsList, parseBlock]
    parseIfAsList = do
      ifres <- parseIf
      return [ifres]

parseIIf :: Parser IExpr
parseIIf = do
  keyword Falsum.Lexer.If
  cond <- parseBExpr
  ifBlock <- parseIIfBlock
  elseBlock <- parseElse
  return $ IIf cond ifBlock elseBlock

  where
    parseElse = do
      keyword Else
      choice [parseIfAsList, parseIIfBlock]
    parseIfAsList = do
      ifres <- parseIIf --must be typed too
      return [Expr (IExpr ifres)]

parseIIfBlock :: Parser [Stmt]
parseIIfBlock = do
  structSymbol LBrace
  modifyState addNewScope
  stmts <- many $ try parseIIfInnerStmt
  lastExpr <- optionMaybe parseIExpr
  case lastExpr of
    Just e -> do
      modifyState removeCurrentScope
      structSymbol RBrace
      return $ stmts ++ [Expr (IExpr e)]
    Nothing       --if IIf was parsed (Only IEXpr without Semicolon at end - sou could be the last)
     -> do
      if length stmts == 0
        then unexpected "Empty IIf block statements"
        else return ()
      let lastStmt = last stmts
      case lastStmt of
        Expr (IExpr IIf{}) -> do
          modifyState removeCurrentScope
          structSymbol RBrace
          return stmts
        _ -> unexpected "Expecting expression at the end of If expression"

parseIIfInnerStmt :: Parser Stmt
parseIIfInnerStmt = choice
                      [parseAnyStmt, Expr <$> (IExpr <$> try parseIIf) -- no semicolon
                                     , parseIf, (Expr <$> try parseExpr) <* structSymbol Semicolon]

parseVCall :: Parser Stmt
parseVCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ (parseExpr <|> sLit) `sepBy` comma
    structSymbol Semicolon
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    checkFnCallParams fnSym fnParams
    case fnSym of
      FnSymbol _ _ Nothing         -> return $ VCall fnSym fnParams
      VariadicFnSymbol _ _ Nothing -> return $ VCall fnSym fnParams
      _                            -> unexpected
                                        "Function return type does not match. Expecting void."

sLit :: Parser Expr
sLit =
  do
    str <- satisfy isStringLiteral
    case token2Lit str of
      ByteString s    -> return $ SExpr (ConstSymbol s String)
      UnicodeString s -> return $ SExpr (ConstSymbol s String)

parseITerm :: Parser IExpr
parseITerm = choice
               [ inParens parseIExpr
               , try parseICall
               , try parseIAssign
               , try parseIVar
               , try parseIIf
               , parseILit
               ]
             <?> "simple int expression"

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
    checkVarType sym (IExpr (IVar sym))
    return $ IVar sym

parseIAssign :: Parser IExpr
parseIAssign =
  do
    varName <- parseSymbolName
    sym <- safeLookupSymbol varName "Missing variable symbol: "
    operator EqSign
    assign <- parseIExpr
    return $ IAssign (LValue sym) assign

parseICall :: Parser IExpr
parseICall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    checkFnCallParams fnSym fnParams
    case fnSym of
      FnSymbol _ _ (Just Int)         -> return $ ICall fnSym fnParams
      VariadicFnSymbol _ _ (Just Int) -> return $ ICall fnSym fnParams
      _                               -> unexpected
                                           "Function return type does not match. Expection int"

parseFTerm :: Parser FExpr
parseFTerm = choice
               [inParens parseFExpr, try parseFCall, try parseFAssign, try parseFVar,
                                                                                      -- , try
                                                                                      -- parseFIf
                                                                                      parseFLit]
             <?> "simple float expression"

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
    getVal (Literal (FloatLit Nothing (Right doubleVal))) = realToFrac doubleVal

parseFVar :: Parser FExpr
parseFVar =
  do
    symbName <- parseSymbolName
    sym <- safeLookupSymbol symbName "Missing symbol: "
    checkVarType sym (FExpr (FVar sym))
    return $ FVar sym

parseFCall :: Parser FExpr
parseFCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    checkFnCallParams fnSym fnParams
    case fnSym of
      FnSymbol _ _ (Just Real)         -> return $ FCall fnSym fnParams
      VariadicFnSymbol _ _ (Just Real) -> return $ FCall fnSym fnParams
      _                                -> unexpected
                                            "Function return type does not match. Expection float"

parseFAssign :: Parser FExpr
parseFAssign =
  do
    varName <- parseSymbolName
    sym <- safeLookupSymbol varName "Missing variable symbol: "
    operator EqSign
    assign <- parseFExpr
    return $ FAssign (LValue sym) assign

parseBTerm :: Parser BExpr
parseBTerm = choice
               [ inParens parseBExpr
               , try parseRelation
               , try parseBCall
               , try parseBAssign
               , try parseBVar
               , parseTrue
               , parseFalse
               ]
             -- , try parseBIf
             <?> "simple bool expression"

bBinaryTable :: [[E.Operator [TokenPos] ParseState Identity BExpr]]
bBinaryTable = [ [prefix Not BNot]
               , [binaryl And (BBinary BAnd)]
               , [binaryl Caret (BBinary BXor)]
               , [binaryl Or (BBinary BOr)]
               , [binaryl DoubleEq (BBinary BEq), binaryl Neq (BBinary BNotEq)]
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
    checkVarType sym (BExpr (BVar sym))
    return $ BVar sym

parseBAssign :: Parser BExpr
parseBAssign =
  do
    varName <- parseSymbolName
    sym <- safeLookupSymbol varName "Missing variable symbol: "
    operator EqSign
    assign <- parseBExpr
    return $ BAssign (LValue sym) assign

parseBCall :: Parser BExpr
parseBCall =
  do
    fnName <- parseSymbolName
    fnParams <- inParens $ parseExpr `sepBy` comma
    fnSym <- safeLookupSymbol fnName "Missing function symbol: "
    checkFnCallParams fnSym fnParams
    case fnSym of
      FnSymbol _ _ (Just Bool)         -> return $ BCall fnSym fnParams
      VariadicFnSymbol _ _ (Just Bool) -> return $ BCall fnSym fnParams
      _                                -> unexpected
                                            "Function return type does not match. Expection bool"

parseRelation :: Parser BExpr
parseRelation = choice [parseIRBinary, parseFRBinary]

parseIRBinary :: Parser BExpr
parseIRBinary =
  do
    expr1 <- parseIExpr
    op <- satisfy isAnyOperator
    expr2 <- parseIExpr
    let rop = parseROp $ extractOp op
    case rop of
      Nothing -> unexpected "Unsupported operator"
      _       -> return ()
    return $ IRBinary (unmaybeOp rop) expr1 expr2

  where
    extractOp (Operator o) = o
    unmaybeOp (Just op) = op

parseFRBinary :: Parser BExpr
parseFRBinary =
  do
    expr1 <- parseFExpr
    op <- satisfy isAnyOperator
    expr2 <- parseFExpr
    let rop = parseROp $ extractOp op
    case rop of
      Nothing -> unexpected "Unsupported operator"
      _       -> return ()
    return $ FRBinary (unmaybeOp rop) expr1 expr2

  where
    extractOp (Operator o) = o
    unmaybeOp (Just op) = op

parseROp :: Operator -> Maybe ROp
parseROp Falsum.Lexer.Less = Just Falsum.AST.Less
parseROp Falsum.Lexer.Leq = Just Falsum.AST.LessEqual
parseROp Falsum.Lexer.Greater = Just Falsum.AST.Greater
parseROp Falsum.Lexer.Geq = Just Falsum.AST.GreaterEqual
parseROp Falsum.Lexer.DoubleEq = Just Falsum.AST.Equal
parseROp Falsum.Lexer.Neq = Just Falsum.AST.NotEqual
parseROp _ = Nothing
