module Falsum.Codegen where

--import Control.Monad hiding (void)
import           Data.Char
import           Data.Word
--import           Debug.Trace
import qualified Falsum.AST                         as F
{-import           Falsum.Lexer                       (backwardLookup,
                                                     forwardLookup)-}
import           LLVM.General.AST                   hiding (GetElementPtr)
import           LLVM.General.AST.AddrSpace
import           LLVM.General.AST.Attribute
import           LLVM.General.AST.CallingConvention
--import  LLVM.General.AST.Instruction hiding (GetElementPtr)
import           LLVM.General.AST.Linkage
-- import LLVM.General.AST.Name import LLVM.General.AST.Operand
import           LLVM.General.AST.Type
import           LLVM.General.AST.Visibility
-- import LLVM.General.AST.DLL as DLL import qualified LLVM.General.AST.ThreadLocalStorage as TLS
import           LLVM.General.AST.Constant          (Constant (Array, Float, GetElementPtr, GlobalReference, Int))
-- TODO for constant expressions instructions import qualified LLVM.General.AST.Constant as C
import           LLVM.General.AST.Float
--import qualified LLVM.General.AST.FloatingPointPredicate as FP
import           LLVM.General.Context
import           LLVM.General.Module                hiding (Module)
import           LLVM.General.PrettyPrint

import           Control.Monad                      hiding (void)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Lazy
import           Data.List

data CodegenState =
       CodegenState
         { blockIdentifierParts :: [Either String Word]
         , registerNumber       :: Word
         }
  deriving Show

initialCodegenState :: CodegenState
initialCodegenState = CodegenState [Right 1] 1

type Codegen = State CodegenState

blockIdentifier :: [Either String Word] -> String
blockIdentifier parts = "_" ++ (intercalate "_" $ map (either id show) $ reverse parts)

{-
blockCounter :: [Either String Word] -> Maybe Word
blockCounter [] = Nothing
blockCounter ((Left _):xs) = blockCounter xs
blockCounter ((Right x):_) = Just x
-}
modifyBlockCounter :: (Word -> Word) -> [Either String Word] -> [Either String Word]
modifyBlockCounter _ [] = []
modifyBlockCounter f ((Left _):xs) = modifyBlockCounter f xs
modifyBlockCounter f ((Right x):xs) = (Right $ f x) : xs

addNamedCounter' :: String -> [Either String Word] -> [Either String Word]
addNamedCounter' name parts = Right 1 : Left name : parts

modifyCodegen :: ([Either String Word] -> [Either String Word]) -> (Word -> Word) -> State CodegenState ()
modifyCodegen a b =
  do
    current <- get
    put $ CodegenState (a . blockIdentifierParts $ current) (b . registerNumber $ current)

addNamedCounter :: String -> State CodegenState ()
addNamedCounter name = modifyCodegen (addNamedCounter' name) id

removeNamedCounter :: State CodegenState ()
removeNamedCounter = modifyCodegen (modifyBlockCounter id . tail) id

currentBlockIdentifier :: State CodegenState String
currentBlockIdentifier = blockIdentifier . blockIdentifierParts <$> get

increaseBlockIdentifier :: State CodegenState ()
increaseBlockIdentifier = modifyCodegen (modifyBlockCounter (+ 1)) id

nextBlockIdentifier :: State CodegenState String
nextBlockIdentifier = get >>= return . evalState (increaseBlockIdentifier >> currentBlockIdentifier)

nextOuterBlockIdentifier :: State CodegenState String
nextOuterBlockIdentifier = get >>= return . evalState (removeNamedCounter >> nextBlockIdentifier)

lastUsedRegisterNumber :: State CodegenState Word
lastUsedRegisterNumber = (subtract 1 . registerNumber) <$> get

currentRegisterNumber :: State CodegenState Word
currentRegisterNumber = registerNumber <$> get

increaseRegisterNumber :: State CodegenState ()
increaseRegisterNumber = modifyCodegen id (+ 1)

claimRegisterNumber :: State CodegenState Word
claimRegisterNumber = currentRegisterNumber <* increaseRegisterNumber

type SymbolToRegisterTable = [(F.Symbol, Name)]

debug :: a -> String -> a
debug = const --flip trace

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

defaultInstrMeta :: InstructionMetadata
defaultInstrMeta = []

defaultAlignment :: Word32
defaultAlignment = 0

align1 :: Word32
align1 = 1

align4 :: Word32
align4 = 4

strPointerType :: Type
strPointerType = PointerType i8 (AddrSpace 0)

strArrayType :: Int -> Type
strArrayType len = ArrayType (toEnum len) i8

i32Lit :: Integer -> Constant
i32Lit = Int 32

charLit :: Char -> Constant
charLit c = Int 8 $ toInteger (ord c)

strLit :: String -> Constant
strLit s = Array (strArrayType (length s)) $ map charLit s

f32Lit :: Float -> Constant
f32Lit = Float . Single

defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

block :: String -> [Named Instruction] -> (Named Terminator) -> BasicBlock
block name instructions terminator = BasicBlock  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L75

                                       (Name name)
                                       instructions
                                       terminator

-- TODO Generate all integer expressions, not just literal
generateIExpression :: F.IExpr -> Codegen [Named Instruction]
-- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateIExpression (F.ILit val) =
  do
    reg <- UnName <$> claimRegisterNumber
    return
      [ reg := Alloca i32 Nothing align4 defaultInstrMeta
      , Do $ Store False (LocalReference i32 reg) (ConstantOperand $ i32Lit val) Nothing align4
               defaultInstrMeta
      ]
generateIExpression (F.IVar (F.VarSymbol name F.Int)) =
  do
    reg <- UnName <$> claimRegisterNumber
    return [reg := Load False (LocalReference i32 (Name name)) Nothing align4 defaultInstrMeta]
generateIExpression (F.IVar (F.GlobalVarSymbol name F.Int)) =
  do
    reg <- UnName <$> claimRegisterNumber
    return
      [ reg := Load False (ConstantOperand (GlobalReference i32 (Name name))) Nothing align4
                 defaultInstrMeta
      ]
-- TODO use a const literal instead of a reference, AST has to/should be changed
generateIExpression (F.IVar (F.ConstSymbol name F.Int)) =
  do
    reg <- UnName <$> claimRegisterNumber
    return
      [ reg := Load False (ConstantOperand (GlobalReference i32 (Name name))) Nothing align4
                 defaultInstrMeta
      ]
generateIExpression (F.INeg expr) =
  do
    instrs <- generateIExpression expr
    resultReg <- UnName <$> lastUsedRegisterNumber
    reg <- UnName <$> claimRegisterNumber
    return $ instrs ++ [ reg := Sub
                                  False
                                  False
                                  (ConstantOperand $ i32Lit 0)
                                  (LocalReference i32 resultReg)
                                  defaultInstrMeta
                       ]
generateIExpression (F.IBinary op leftExpr rightExpr) =
  do
    leftInstrs <- generateIExpression leftExpr
    leftResultReg <- UnName <$> lastUsedRegisterNumber
    rightInstrs <- generateIExpression rightExpr
    rightResultReg <- UnName <$> lastUsedRegisterNumber
    reg <- UnName <$> claimRegisterNumber
    return $ leftInstrs ++
             rightInstrs ++
             [reg := opInstr (LocalReference i32 leftResultReg) (LocalReference i32 rightResultReg)]

  where
    opInstr l r =
      case op of
        F.IPlus  -> Add False False l r defaultInstrMeta
        F.IMinus -> Sub False False l r defaultInstrMeta
        F.IMult  -> Mul False False l r defaultInstrMeta
        F.IDiv   -> SDiv True l r defaultInstrMeta -- TODO is it right?
        _        -> undefined -- TODO other operations
generateIExpression (F.ICall symbol argExprs) =
  case symbol of
    F.FnSymbol name argTypes retType         -> genCall name argTypes retType False argExprs
    F.VariadicFnSymbol name argTypes retType -> genCall name argTypes retType True argExprs
generateIExpression (F.IAssign (F.LValue (F.VarSymbol name F.Int)) expr) =
  do
    instrs <- generateIExpression expr
    resultReg <- UnName <$> lastUsedRegisterNumber
    reg <- UnName <$> claimRegisterNumber
    return $ instrs ++ [ reg := Store
                                  False
                                  (LocalReference i32 (Name name))
                                  (LocalReference i32 resultReg)
                                  Nothing
                                  align4
                                  defaultInstrMeta
                       ]

genCall :: String -> [F.ValueType] -> (Maybe F.ValueType) -> Bool -> [F.Expr] -> Codegen [Named Instruction]
genCall n aTys rTy isVararg argExprs = do
  argsWithInstructions <- mapM passArg argExprs
  instructions <- return $ concatMap fst argsWithInstructions
  reg <- UnName <$> currentRegisterNumber
  when (rTy /= Nothing) increaseRegisterNumber
  call <- return
            [ (if rTy == Nothing
                 then Do
                 else (reg :=)) $ Call
                                    Nothing
                                    C
                                    []
                                    (Right $ ConstantOperand $ GlobalReference
                                                                 (FunctionType
                                                                    (genTy rTy)
                                                                    (map (genTy . Just) aTys)
                                                                    isVararg)
                                                                 (Name n))
                                    (map snd argsWithInstructions)
                                    [Left $ GroupID 0]
                                    defaultInstrMeta
            ]
  return $ instructions ++ call

-- TODO Generate all expressions
generateExpression :: F.Expr -> Codegen [Named Instruction]
generateExpression (F.IExpr expr) = generateIExpression expr

passArg :: F.Expr -> Codegen ([Named Instruction], (Operand, [ParameterAttribute]))
passArg expr
  | (F.IExpr (F.IVar (F.VarSymbol name F.Int))) <- expr = genPassing name i32
  | (F.FExpr (F.FVar (F.VarSymbol name F.Real))) <- expr = genPassing name float
  | (F.BExpr (F.BVar (F.VarSymbol name F.Bool))) <- expr = genPassing name i1
  -- SExpr (VarSymbol "format" String)
  | (F.SExpr (F.ConstSymbol name F.String)) <- expr =
      return
        ([], (ConstantOperand
                (GetElementPtr True (GlobalReference strPointerType (Name name))
                   [i32Lit 0, i32Lit 0]), []))
  where
    genPassing name ty =
      do
        freeRegister <- claimRegisterNumber
        return
          ([ UnName freeRegister := Load False (LocalReference ty (Name name)) Nothing align4
                                      defaultInstrMeta
           ], (LocalReference ty (UnName freeRegister), []))

-- TODO Generate all statements
generateStatement :: F.Stmt -> Codegen [Named Instruction]
-- (VarLet (VarSymbol "b" Int) (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateStatement stmt
  | (F.VarLetStmt (F.VarLet (F.VarSymbol name F.Int) expr)) <- stmt =
      do
        allocInstr <- return (Name name := Alloca i32 Nothing align4 defaultInstrMeta)
        initInstrs <- generateExpression expr
        return $ allocInstr : initInstrs
  -- Expr (...)
  | (F.Expr e) <- stmt = generateExpression e
  -- VCall (FnSymbol "foo" Nothing) [args]
  | (F.VCall (F.FnSymbol name argTypes _) args) <- stmt = genCall name argTypes Nothing False args
  | (F.VCall (F.VariadicFnSymbol name argTypes _) args) <- stmt = genCall name argTypes Nothing True
                                                                    args

generateStatements :: [F.Stmt] -> Codegen [Named Instruction]
generateStatements stmts =
  do
    gens <- mapM generateStatement stmts
    return $ concat gens

genTerm :: Maybe Operand -> Named Terminator
genTerm o = Do $ Ret o defaultInstrMeta

-- TODO
generateReturnTerminator :: F.Stmt -> Codegen (Named Terminator)
generateReturnTerminator (F.Return e) = return . genTerm . fmap conv $ e
  where
    conv (F.IExpr (F.ILit l)) = ConstantOperand $ i32Lit l
    conv _ = undefined -- TODO

generateReturnTerminator stmt = error
                                  ("'generateReturnTerminator' accepts only Return statements, '" ++
                                   show stmt ++
                                   "' given.")

stmtsInAST :: SymbolToRegisterTable -> String -> [F.Stmt] -> Codegen [BasicBlock]
stmtsInAST _ blockName statements =
  do
    stmts <- generateStatements $ init statements
    terminator <- generateReturnTerminator $ last statements
    return [block blockName stmts terminator]

constLetInAST :: F.ConstLet -> Global
{-|
  ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
  ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
  ConstLet (ConstSymbol "format" String) (StringVal "test %d")
-}
constLetInAST (F.ConstLet sym val)    -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

 =
  case (sym, val) of
    ((F.ConstSymbol s F.Int), (F.IntVal v)) -> genGVar False (enrich s) i32 $ i32Lit $ toInteger v
    ((F.ConstSymbol s F.Real), (F.RealVal v)) -> genGVar False (enrich s) float $ f32Lit v
    ((F.ConstSymbol s F.String), (F.StringVal v)) -> genGVar True s (strArrayType (length v)) $ strLit
                                                                                                  v
  where
    enrich = id --const "const"

constLetListInAST :: [F.ConstLet] -> [Global]
constLetListInAST = map constLetInAST

genGVar :: Bool -> String -> Type -> Constant -> Global
genGVar isStr name ty val = GlobalVariable  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

                              (Name name)
                              (if isStr
                                 then Private
                                 else Internal)
                              Default
                              Nothing
                              Nothing
                              defaultAddrSpace
                              False
                              False
                              ty
                              (Just val)
                              Nothing
                              Nothing
                              (if isStr
                                 then align1
                                 else align4)

staticVarLetInAST :: F.VarLet -> Global
{-|
  VarLet (GlobalVarSymbol "M" Int) (IExpr (ILit 5))
  VarLet (GlobalVarSymbol "Moo" Real) (FExpr (FLit 55.5)
-}
-- TODO bool
staticVarLetInAST varLet =
  case varLet of
    (F.VarLet (F.GlobalVarSymbol s F.Int) (F.IExpr (F.IAssign _ (F.ILit v)))) -> genGVar False s i32 $ i32Lit $ toInteger
                                                                                                                  v
    (F.VarLet (F.GlobalVarSymbol s F.Real) (F.FExpr (F.FAssign _ (F.FLit v)))) -> genGVar False s
                                                                                    float $ f32Lit v

staticVarLetListInAST :: [F.VarLet] -> [Global]
staticVarLetListInAST = map staticVarLetInAST

formalArgs :: F.Symbol -> Parameter
formalArgs (F.VarSymbol name ty) = Parameter (genTy . Just $ ty) (Name name) []

genParam :: F.ValueType -> Parameter
genParam ty = Parameter (genTy . Just $ ty) (Name "") []

genTy :: (Maybe F.ValueType) -> Type
genTy ty =
  case ty of
    Nothing       -> void
    Just F.Int    -> i32
    Just F.Real   -> float
    Just F.Bool   -> i1
    Just F.String -> strPointerType

genFn :: Type -> String -> [Parameter] -> Bool -> [BasicBlock] -> Global
genFn ty name args isVararg body = Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                                     External
                                     Default
                                     Nothing
                                     C
                                     []
                                     ty
                                     (Name name)
                                     (args, isVararg)
                                     [Left $ GroupID 0]
                                     Nothing
                                     Nothing
                                     defaultAlignment
                                     Nothing
                                     Nothing
                                     body
                                     Nothing

fnLetInAST :: F.FnLet -> Global
-- DeclareFnLet (FnSymbol "printf" Nothing) [VarSymbol "format" String] vararg
fnLetInAST (F.DeclareFnLet symbol)
  | (F.FnSymbol name argTypes retType) <- symbol =
      genFn (genTy retType) name (map genParam argTypes) False []
  | (F.VariadicFnSymbol name argTypes retType) <- symbol =
      genFn (genTy retType) name (map genParam argTypes) True []

fnLetInAST fnLet = evalState (fnLetInAST' fnLet) initialCodegenState

withSimpleTerminator :: [Named Instruction] -> Codegen BasicBlock
withSimpleTerminator instrs =
  do
    currentId <- currentBlockIdentifier
    afterId <- nextBlockIdentifier
    return $ block currentId instrs $ Do $ Br (Name afterId) defaultInstrMeta

joinBlock :: Codegen BasicBlock
joinBlock =
  do
    currentId <- currentBlockIdentifier
    outerId <- nextOuterBlockIdentifier
    return $ block currentId [] $ Do $ Br (Name outerId) defaultInstrMeta

fnLetInAST' :: F.FnLet -> Codegen Global
-- FnLet (FnSymbol "name" Nothing) [args] [stmts]
fnLetInAST' (F.FnLet (F.FnSymbol name _ retType) args statements) =
  do
    argsToRegs <- unzip <$> mapM argToReg args
    codeArgsToRegs <- return $ fst argsToRegs
    table <- return $ snd argsToRegs
    prologueBlock <- withSimpleTerminator (concat codeArgsToRegs)
    increaseBlockIdentifier
    entryBlockId <- currentBlockIdentifier
    bodyBlocks <- stmtsInAST table entryBlockId statements
    return $ genFn (genTy retType) name (map formalArgs args) False $ prologueBlock : bodyBlocks

argToReg :: F.Symbol -> Codegen ([Named Instruction], (F.Symbol, Name))
argToReg symbol =
  do
    regNumber <- claimRegisterNumber
    return $ argToReg' symbol regNumber

argToReg' :: F.Symbol -> Word -> ([Named Instruction], (F.Symbol, Name))
argToReg' symbol regNumber = (argToRegInstructions symbol (UnName regNumber), (symbol, (UnName
                                                                                          regNumber)))
  where
    argToRegInstructions sym reg
      | (F.VarSymbol _ F.String) <- sym = undefined -- we don't support string values
      | (F.VarSymbol name t) <- sym =
          let ty = genTy . Just $ t
          in [ reg := Alloca ty Nothing align4 defaultInstrMeta
             , Do $ Store
                      False
                      (LocalReference ty reg)
                      (LocalReference ty (Name name))
                      Nothing
                      align4
                      defaultInstrMeta
             ]

fnLetListInAST :: [F.FnLet] -> [Global]
fnLetListInAST = map fnLetInAST

programInAST :: F.Program -> [Global]
programInAST (F.Program constLetList staticVarLetList fnLetList mainLet) = staticVarLetListInAST
                                                                             staticVarLetList ++
                                                                           constLetListInAST
                                                                             constLetList ++
                                                                           fnLetListInAST fnLetList ++
                                                                           [fnLetInAST mainLet]

defaultfunctionAttributes :: Definition
defaultfunctionAttributes = FunctionAttributes (GroupID 0) [NoUnwind, UWTable]

topLevelDefs :: F.Program -> [Definition]
topLevelDefs program = [defaultfunctionAttributes] ++ fmap GlobalDefinition (programInAST program)

{-|
  TODO Set filename as module name
  TODO(optional) Set module DataLayout
  TODO(optional) Set module TargetTriple
-}
moduleInAST :: String -> F.Program -> Module
moduleInAST name program = highLevelModule `debug` showPretty highLevelModule
  where
    highLevelModule = Module name Nothing Nothing $ topLevelDefs program

{-main =
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            ast <- LLVMMod.moduleAST mod
            putStrLn $ LLVMPP.showPretty ast
            liftError $ LLVMMod.withModuleFromAST ctx ast$ \mod -> do
                putStrLn "Success!"

    where
        file = LLVMMod.File "Rx-linked-cg.bc"-}
asm :: String -> F.Program -> IO String
asm name program = withContext $ \ctx ->
  liftError $ withModuleFromAST ctx (moduleInAST name program) moduleLLVMAssembly

codegen :: String -> F.Program -> IO ()
codegen name program = do
  --putStrLn $ show problem
  llvmIR <- asm name program
  putStrLn llvmIR
