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

nextBlockIdentifier :: State CodegenState Name
nextBlockIdentifier = get >>= return .
                              Name .
                              evalState (increaseBlockIdentifier >> currentBlockIdentifier)

nextOuterBlockIdentifier :: State CodegenState Name
nextOuterBlockIdentifier = get >>= return . evalState (removeNamedCounter >> nextBlockIdentifier)

nextInnerBlockIdentifier :: String -> State CodegenState Name
nextInnerBlockIdentifier counterName = get >>= return .
                                               Name .
                                               evalState
                                                 (addNamedCounter counterName >> currentBlockIdentifier)

lastUsedRegister :: State CodegenState Name
lastUsedRegister = (UnName . subtract 1 . registerNumber) <$> get

currentRegister :: State CodegenState Name
currentRegister = (UnName . registerNumber) <$> get

increaseRegisterNumber :: State CodegenState ()
increaseRegisterNumber = modifyCodegen id (+ 1)

claimRegister :: State CodegenState Name
claimRegister = currentRegister <* increaseRegisterNumber

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

i1Lit :: Bool -> Constant
i1Lit b = Int 1 i
  where
    i = (if b
           then 1
           else 0)

i32Lit :: Integer -> Constant
i32Lit = Int 32

charLit :: Char -> Constant
charLit c = Int 8 $ toInteger (ord c)

strLit :: String -> Constant
strLit s = Array (strArrayType (length s)) $ map charLit s

floatLit :: Float -> Constant
floatLit = Float . Single

defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

block :: String -> [Named Instruction] -> (Named Terminator) -> BasicBlock
block name instructions terminator = BasicBlock  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L75

                                       (Name name)
                                       instructions
                                       terminator

generateGAlloca :: Type -> Codegen ([BasicBlock], Name)
generateGAlloca ty =
  do
    reg <- claimRegister
    bl <- simpleBlock [reg := Alloca ty Nothing align4 defaultInstrMeta]
    return $ (bl, reg)

generateGSet :: Name -> Name -> Type -> Codegen [BasicBlock]
generateGSet regTo regFrom ty = simpleBlock
                                  [ Do $ Store
                                           False
                                           (LocalReference ty regTo)
                                           (LocalReference ty regFrom)
                                           Nothing
                                           align4
                                           defaultInstrMeta
                                  ]

generateGLit :: a -> Type -> (a -> Constant) -> Codegen [BasicBlock]
generateGLit val ty litFn =
  do
    reg <- claimRegister
    simpleBlock
      [ reg := Alloca ty Nothing align4 defaultInstrMeta
      , Do $ Store False (LocalReference ty reg) (ConstantOperand $ litFn val) Nothing align4
               defaultInstrMeta
      ]

generateGVar :: Name -> (Name -> Operand) -> Codegen [BasicBlock]
generateGVar varName opFn =
  do
    reg <- claimRegister
    simpleBlock [reg := Load False (opFn varName) Nothing align4 defaultInstrMeta]

generateGNeg :: a -> (a -> Codegen [BasicBlock]) -> (Name -> Operand) -> (Operand -> Instruction) -> Codegen [BasicBlock]
generateGNeg expr genExpr opFn negFn =
  do
    instrs <- genExpr expr
    resultReg <- lastUsedRegister
    reg <- claimRegister
    bl <- simpleBlock [reg := negFn (opFn resultReg)]
    return $ instrs ++ bl

generateGCall :: F.Symbol -> [F.Expr] -> Codegen [BasicBlock]
generateGCall symbol argExprs =
  case symbol of
    F.FnSymbol name argTypes retType         -> genCall name argTypes retType False argExprs
    F.VariadicFnSymbol name argTypes retType -> genCall name argTypes retType True argExprs

generateGBinary :: (Operand -> Operand -> Instruction) -> (Name -> Operand) -> a -> a -> (a -> Codegen [BasicBlock]) -> Codegen [BasicBlock]
generateGBinary opInstr opFn leftExpr rightExpr genExpr =
  do
    leftInstrs <- genExpr leftExpr
    leftResultReg <- lastUsedRegister
    rightInstrs <- genExpr rightExpr
    rightResultReg <- lastUsedRegister
    reg <- claimRegister
    bl <- simpleBlock [reg := opInstr (opFn leftResultReg) (opFn rightResultReg)]
    return $ leftInstrs ++
             rightInstrs ++
             bl

generateGAssign :: Name -> (Name -> Operand) -> a -> (a -> Codegen [BasicBlock]) -> Codegen [BasicBlock]
generateGAssign varName opFn expr genExpr =
  do
    instrs <- genExpr expr
    resultReg <- lastUsedRegister
    reg <- claimRegister
    bl <- simpleBlock
            [reg := Store False (opFn varName) (opFn resultReg) Nothing align4 defaultInstrMeta]
    return $ instrs ++ bl

generateGIf :: F.BExpr -> Type -> [F.Stmt] -> [F.Stmt] -> Codegen [BasicBlock]
generateGIf cond ty thenStmts elseStmts =
  do
    condBlock <- generateBExpression cond
    resultReg <- lastUsedRegister
    allocBlock <- generateGAlloca ty
    p <- nextInnerBlockIdentifier "then"
    n <- nextInnerBlockIdentifier "else"
    branchingBlock <- condBranchBlock (LocalReference i1 resultReg) p n
    addNamedCounter "then"
    pBr <- generateStatements thenStmts
    pResult <- currentRegister
    pSet <- generateGSet (snd allocBlock) pResult ty
    pJoin <- joinBlock
    removeNamedCounter
    addNamedCounter "else"
    nBr <- generateStatements elseStmts
    nResult <- currentRegister
    nSet <- generateGSet (snd allocBlock) nResult ty
    nJoin <- joinBlock
    removeNamedCounter
    increaseBlockIdentifier
    return $ condBlock ++
             fst allocBlock ++
             branchingBlock ++
             pBr ++
             pSet ++
             [pJoin] ++
             nBr ++
             nSet ++
             [nJoin]

-- TODO IIf
generateIExpression :: F.IExpr -> Codegen [BasicBlock]
generateIExpression (F.ILit val) = generateGLit val i32 i32Lit
generateIExpression (F.IVar (F.VarSymbol name F.Int)) = generateGVar (Name name)
                                                          (LocalReference i32)
generateIExpression (F.IVar (F.GlobalVarSymbol name F.Int)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                              i32)
-- TODO use a const literal instead of a reference, AST has to/should be changed
generateIExpression (F.IVar (F.ConstSymbol name F.Int)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                          i32)
generateIExpression (F.INeg expr) = generateGNeg expr generateIExpression (LocalReference i32)
                                      (\o ->
                                         Sub False False (ConstantOperand $ i32Lit 0) o
                                           defaultInstrMeta)
generateIExpression (F.IBinary op leftExpr rightExpr) = generateGBinary
                                                          opInstr
                                                          (LocalReference i32)
                                                          leftExpr
                                                          rightExpr
                                                          generateIExpression
  where
    opInstr l r =
      case op of
        F.IPlus  -> Add False False l r defaultInstrMeta
        F.IMinus -> Sub False False l r defaultInstrMeta
        F.IMult  -> Mul False False l r defaultInstrMeta
        F.IDiv   -> SDiv True l r defaultInstrMeta -- TODO is it right?
        F.IMod   -> SRem l r defaultInstrMeta
        F.IAnd   -> And l r defaultInstrMeta
        F.IOr    -> Or l r defaultInstrMeta
        F.IXor   -> Xor l r defaultInstrMeta
generateIExpression (F.ICall symbol argExprs) = generateGCall symbol argExprs
generateIExpression (F.IAssign (F.LValue (F.VarSymbol name F.Int)) expr) = generateGAssign
                                                                             (Name name)
                                                                             (LocalReference i32)
                                                                             expr
                                                                             generateIExpression
generateIExpression (F.IIf cond p n) = generateGIf cond i32 p n

-- TODO FIf
generateFExpression :: F.FExpr -> Codegen [BasicBlock]
generateFExpression (F.FLit val) = generateGLit val float floatLit
generateFExpression (F.FVar (F.VarSymbol name F.Real)) = generateGVar (Name name)
                                                           (LocalReference float)
generateFExpression (F.FVar (F.GlobalVarSymbol name F.Real)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                               float)
-- TODO use a const literal instead of a reference, AST has to/should be changed
generateFExpression (F.FVar (F.ConstSymbol name F.Real)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                           float)
generateFExpression (F.FNeg expr) = generateGNeg expr generateFExpression (LocalReference float)
                                      (\o ->
                                         Sub False False (ConstantOperand $ floatLit 0) o
                                           defaultInstrMeta)
generateFExpression (F.FBinary op leftExpr rightExpr) = generateGBinary
                                                          opInstr
                                                          (LocalReference float)
                                                          leftExpr
                                                          rightExpr
                                                          generateFExpression
  where
    opInstr l r =
      case op of
        F.FPlus  -> Add False False l r defaultInstrMeta
        F.FMinus -> Sub False False l r defaultInstrMeta
        F.FMult  -> Mul False False l r defaultInstrMeta
        F.FDiv   -> SDiv True l r defaultInstrMeta -- TODO is it right?
generateFExpression (F.FCall symbol argExprs) = generateGCall symbol argExprs
generateFExpression (F.FAssign (F.LValue (F.VarSymbol name F.Real)) expr) = generateGAssign
                                                                              (Name name)
                                                                              (LocalReference float)
                                                                              expr
                                                                              generateFExpression
generateFExpression (F.FIf cond p n) = generateGIf cond float p n

-- TODO BIf, relation operators (IRBinary, FRBinary)
generateBExpression :: F.BExpr -> Codegen [BasicBlock]
generateBExpression (F.BLit val) = generateGLit val i1 i1Lit
generateBExpression (F.BVar (F.VarSymbol name F.Bool)) = generateGVar (Name name)
                                                           (LocalReference i1)
generateBExpression (F.BVar (F.GlobalVarSymbol name F.Bool)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                               i1)
-- TODO use a const literal instead of a reference, AST has to/should be changed
generateBExpression (F.BVar (F.ConstSymbol name F.Bool)) = generateGVar (Name name) $ ConstantOperand . (GlobalReference
                                                                                                           i1)
generateBExpression (F.BNot expr) = generateGNeg expr generateBExpression (LocalReference i1)
                                      (\o -> Xor (ConstantOperand $ i1Lit True) o defaultInstrMeta)
generateBExpression (F.BBinary F.BEq leftExpr rightExpr) =
  do
    leftInstrs <- generateBExpression leftExpr
    leftResultReg <- lastUsedRegister
    rightInstrs <- generateBExpression rightExpr
    rightResultReg <- lastUsedRegister
    reg <- claimRegister
    regNeg <- claimRegister
    bl <- simpleBlock
            [ reg := Xor (LocalReference i1 leftResultReg) (LocalReference i1 rightResultReg)
                       defaultInstrMeta
            , regNeg := Xor (ConstantOperand $ i1Lit True) (LocalReference i1 reg) defaultInstrMeta
            ]
    return $ leftInstrs ++
             rightInstrs ++
             bl
generateBExpression (F.BBinary op leftExpr rightExpr) = generateGBinary
                                                          opInstr
                                                          (LocalReference i1)
                                                          leftExpr
                                                          rightExpr
                                                          generateBExpression
  where
    opInstr l r =
      case op of
        F.BAnd   -> And l r defaultInstrMeta
        F.BOr    -> Or l r defaultInstrMeta
        F.BNotEq -> Xor l r defaultInstrMeta
generateBExpression (F.IRBinary _ _ _) = simpleBlock [] -- TODO
generateBExpression (F.FRBinary _ _ _) = simpleBlock [] -- TODO
generateBExpression (F.BCall symbol argExprs) = generateGCall symbol argExprs
generateBExpression (F.BAssign (F.LValue (F.VarSymbol name F.Bool)) expr) = generateGAssign
                                                                              (Name name)
                                                                              (LocalReference i1)
                                                                              expr
                                                                              generateBExpression
generateBExpression (F.BIf cond p n) = generateGIf cond i1 p n

genCall :: String -> [F.ValueType] -> (Maybe F.ValueType) -> Bool -> [F.Expr] -> Codegen [BasicBlock]
genCall n aTys rTy isVararg argExprs = do
  argsWithInstructions <- mapM passArg argExprs
  instructions <- return $ concatMap fst argsWithInstructions
  reg <- currentRegister
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
  bl <- simpleBlock call
  return $ instructions ++ bl

generateExpression :: F.Expr -> Codegen [BasicBlock]
generateExpression (F.IExpr expr) = generateIExpression expr
generateExpression (F.FExpr expr) = generateFExpression expr
generateExpression (F.BExpr expr) = generateBExpression expr

passArg :: F.Expr -> Codegen ([BasicBlock], (Operand, [ParameterAttribute]))
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
        freeRegister <- claimRegister
        bl <- simpleBlock
                [ freeRegister := Load False (LocalReference ty (Name name)) Nothing align4
                                    defaultInstrMeta
                ]
        return (bl, (LocalReference ty freeRegister, []))

-- TODO Generate all statements
generateStatement :: F.Stmt -> Codegen [BasicBlock]
-- (VarLet (VarSymbol "b" Int) (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateStatement stmt
  | (F.VarLetStmt (F.VarLet (F.VarSymbol name F.Int) expr)) <- stmt =
      do
        allocInstr <- return (Name name := Alloca i32 Nothing align4 defaultInstrMeta)
        bl <- simpleBlock [allocInstr]
        initInstrs <- generateExpression expr
        return $ bl ++ initInstrs
  -- Expr (...)
  | (F.Expr e) <- stmt = generateExpression e
  -- VCall (FnSymbol "foo" Nothing) [args]
  | (F.VCall (F.FnSymbol name argTypes _) args) <- stmt = genCall name argTypes Nothing False args
  | (F.VCall (F.VariadicFnSymbol name argTypes _) args) <- stmt = genCall name argTypes Nothing True
                                                                    args
  | (F.Return Nothing) <- stmt = returnBlock undefined void
  | (F.Return (Just e)) <- stmt =
      case e of
        (F.IExpr expr) -> do
          exprBl <- generateIExpression expr
          resultReg <- lastUsedRegister
          ret <- returnBlock resultReg i32
          return $ exprBl ++ ret
        (F.FExpr expr) -> do
          exprBl <- generateFExpression expr
          resultReg <- lastUsedRegister
          ret <- returnBlock resultReg float
          return $ exprBl ++ ret
        (F.BExpr expr) -> do
          exprBl <- generateBExpression expr
          resultReg <- lastUsedRegister
          ret <- returnBlock resultReg i1
          return $ exprBl ++ ret
  | (F.If cond thenBr elseBr) <- stmt =
      do
        condBlock <- generateBExpression cond
        resultReg <- lastUsedRegister
        next <- nextBlockIdentifier
        p <- nextInnerBlockIdentifier "then"
        n <- nextInnerBlockIdentifier "else"
        --fail $ show next ++ "|" ++ show p ++ "|" ++ show n
        branchingBlock <- condBranchBlock (LocalReference i1 resultReg) p
                            (if elseBr == Nothing
                               then next
                               else n)
        addNamedCounter "then"
        pBr <- generateStatements thenBr
        pJoin <- joinBlock
        removeNamedCounter
        case elseBr of
          Nothing -> do
            increaseBlockIdentifier
            return $ condBlock ++ branchingBlock ++ pBr ++ [pJoin]
          Just el -> do
            addNamedCounter "else"
            nBr <- generateStatements el
            nJoin <- joinBlock
            removeNamedCounter
            increaseBlockIdentifier
            return $ condBlock ++ branchingBlock ++ pBr ++ [pJoin] ++ nBr ++ [nJoin]
  | (F.Loop stmts) <- stmt =
      do
        loopStart <- nextInnerBlockIdentifier "loop"
        jmpIn <- jumpBlock loopStart
        addNamedCounter "loop"
        blocks <- generateStatements stmts
        jmp <- jumpBlock loopStart
        removeNamedCounter
        increaseBlockIdentifier
        return $ [jmpIn] ++ blocks ++ [jmp]
  | (F.While cond stmts) <- stmt =
      do
        whileStart <- nextInnerBlockIdentifier "while"
        jmpIn <- jumpBlock whileStart
        addNamedCounter "while"
        condBlock <- generateBExpression cond
        resultReg <- lastUsedRegister
        whileEnd <- nextOuterBlockIdentifier
        whileInner <- nextInnerBlockIdentifier "whileBody"
        branchingBlock <- condBranchBlock (LocalReference i1 resultReg) whileInner whileEnd
        addNamedCounter "whileBody"
        whileBody <- generateStatements stmts
        jmp <- jumpBlock whileStart
        removeNamedCounter
        removeNamedCounter
        increaseBlockIdentifier
        return $ [jmpIn] ++ condBlock ++ branchingBlock ++ whileBody ++ [jmp]

-- | otherwise = fail $ show stmt
generateStatements :: [F.Stmt] -> Codegen [BasicBlock]
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
stmtsInAST _ _ statements = generateStatements statements

{-do
    stmts <- generateStatements $ init statements
    terminator <- generateReturnTerminator $ last statements
    -return [block blockName stmts terminator]-}
{-|
  ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
  ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
  ConstLet (ConstSymbol "format" String) (StringVal "test %d")
-}
constLetInAST :: F.ConstLet -> Global
constLetInAST (F.ConstLet sym val)    -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

 =
  case (sym, val) of
    ((F.ConstSymbol s F.Int), (F.IntVal v)) -> genGVar False (enrich s) i32 $ i32Lit $ toInteger v
    ((F.ConstSymbol s F.Real), (F.RealVal v)) -> genGVar False (enrich s) float $ floatLit v
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
                                                                                    float $ floatLit
                                                                                              v

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
    return $ block currentId instrs $ Do $ Br afterId defaultInstrMeta

simpleBlock :: [Named Instruction] -> Codegen [BasicBlock]
simpleBlock instrs =
  do
    bl <- withSimpleTerminator instrs
    increaseBlockIdentifier
    return [bl]

-- TODO returning values
returnBlock :: Name -> Type -> Codegen [BasicBlock]
returnBlock name ty =
  do
    currentId <- currentBlockIdentifier
    --increaseBlockIdentifier
    return $ [block currentId [] $ Do $ Ret
                                          (if ty == void
                                             then Nothing
                                             else Just (LocalReference ty name))
                                          defaultInstrMeta]

condBranchBlock :: Operand -> Name -> Name -> Codegen [BasicBlock]
condBranchBlock o thenBr elseBr =
  do
    currentId <- currentBlockIdentifier
    return $ [block currentId [] $ Do $ CondBr o thenBr elseBr defaultInstrMeta]

joinBlock :: Codegen BasicBlock
joinBlock =
  do
    currentId <- currentBlockIdentifier
    outerId <- nextOuterBlockIdentifier
    return $ block currentId [] $ Do $ Br outerId defaultInstrMeta

jumpBlock :: Name -> Codegen BasicBlock
jumpBlock label =
  do
    currentId <- currentBlockIdentifier
    return $ block currentId [] $ Do $ Br label defaultInstrMeta

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
    reg <- claimRegister
    case symbol of
      (F.VarSymbol _ F.String) -> fail "Strings are not supported." -- we don't support string values
      _                        -> return $ argToReg' symbol reg

argToReg' :: F.Symbol -> Name -> ([Named Instruction], (F.Symbol, Name))
argToReg' symbol reg = (argToRegInstructions symbol reg, (symbol, reg))
  where
    argToRegInstructions (F.VarSymbol name t) r =
      let ty = genTy . Just $ t
      in [ reg := Alloca ty Nothing align4 defaultInstrMeta
         , Do $ Store False (LocalReference ty r) (LocalReference ty (Name name)) Nothing align4
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
