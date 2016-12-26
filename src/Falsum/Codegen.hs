module Falsum.Codegen where

import           Data.Char
import           Data.Word
import           Debug.Trace
import           Falsum.AST
import           Falsum.Lexer                       (backwardLookup,
                                                     forwardLookup)
import qualified LLVM.General.AST                   as AST
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Attribute         as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Instruction       as I
import qualified LLVM.General.AST.Linkage           as L
import qualified LLVM.General.AST.Name              as N
import qualified LLVM.General.AST.Operand           as O
import qualified LLVM.General.AST.Type              as T
import qualified LLVM.General.AST.Visibility        as V
-- import qualified LLVM.General.AST.DLL as DLL import qualified LLVM.General.AST.ThreadLocalStorage
-- as TLS
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.Float             as F
--import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.Context               as CTX
import qualified LLVM.General.Module                as MOD
import qualified LLVM.General.PrettyPrint           as PP

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Lazy

data CodegenState = CodegenState { blockNumber :: Integer, registerNumber :: Integer }

initialCodegenState :: CodegenState
initialCodegenState = CodegenState 1 1

type Codegen = State CodegenState

currentBlockPrefix :: State CodegenState String
currentBlockPrefix =
  do
    current <- get
    return $ "_" ++ show (blockNumber current)

increaseBlockPrefix :: State CodegenState ()
increaseBlockPrefix =
  do
    current <- get
    put $ CodegenState (1 + blockNumber current) (registerNumber current)

nextBlockPrefix :: State CodegenState String
nextBlockPrefix = get >>= return . evalState (increaseBlockPrefix >> currentBlockPrefix)

currentRegisterNumber :: State CodegenState Integer
currentRegisterNumber = registerNumber <$> get

increaseRegisterNumber :: State CodegenState ()
increaseRegisterNumber =
  do
    current <- get
    put $ CodegenState (blockNumber current) (1 + registerNumber current)

type SymbolToRegisterTable = [(Symbol, N.Name)]

debug :: a -> String -> a
debug = flip trace

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

simple :: Program
simple = Program
           [ ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
           , ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
           , ConstLet (ConstSymbol "format" String) (StringVal "test %d\00")
           ]
           [ VarLet (GlobalVarSymbol "M" Int) (IExpr (ILit 5))
           , VarLet (GlobalVarSymbol "Moo" Real) (FExpr (FLit 55.5))
           ]
           [ FnLet (FnSymbol "foo" Nothing) []
               [ VarLetStmt
                   (VarLet (VarSymbol "a" Int)
                      (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 21))))
               , Return Nothing
               ]
           , FnLet (FnSymbol "maine" (Just Int)) [VarSymbol "a" Int, VarSymbol "b" Int]
               [VCall (FnSymbol "foo" Nothing) [], Return (Just (IExpr (ILit 0)))]
           , DeclareFnLet (FnSymbol "printf" (Just Int)) [VarSymbol "" String] True
           ]
           (FnLet (FnSymbol "main" Nothing) []
              [ VarLetStmt
                  (VarLet (VarSymbol "a" Int)
                     (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (GlobalVarSymbol "M" Int)))))
              , VarLetStmt
                  (VarLet (VarSymbol "b" Int)
                     (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
              , Expr (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
              , Expr (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
              , Expr (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
              , VCall (FnSymbol "foo" Nothing) []
              , VCall (FnSymbol "maine" Nothing)
                  [IExpr (IVar (VarSymbol "a" Int)), IExpr (IVar (VarSymbol "b" Int))]
              , VCall (FnSymbol "printf" Nothing)
                  [SExpr (GlobalVarSymbol "format" String), IExpr (IVar (VarSymbol "b" Int))]
              , Return Nothing
              ])

defaultInstrMeta :: I.InstructionMetadata
defaultInstrMeta = []

defaultAlignment :: Word32
defaultAlignment = 0

align1 :: Word32
align1 = 1

align4 :: Word32
align4 = 4

strPointerType :: T.Type
strPointerType = T.PointerType T.i8 (AddrSpace 0)

strArrayType :: Int -> T.Type
strArrayType len = T.ArrayType (toEnum len) T.i8

i32Lit :: Integer -> C.Constant
i32Lit = C.Int 32

charLit :: Char -> C.Constant
charLit c = C.Int 8 $ toInteger (ord c)

strLit :: String -> C.Constant
strLit s = C.Array (strArrayType (length s)) $ map charLit s

f32Lit :: Float -> C.Constant
f32Lit = C.Float . F.Single

defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

block :: String -> [I.Named I.Instruction] -> (I.Named I.Terminator) -> AST.BasicBlock
block name instructions terminator = AST.BasicBlock  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L75

                                       (AST.Name name)
                                       instructions
                                       terminator

body :: String -> [I.Named I.Instruction] -> (I.Named I.Terminator) -> [AST.BasicBlock]
body name instructions terminator = [block name instructions terminator]

-- TODO Generate all integer expressions, not just literal
generateIExpression :: IExpr -> Codegen [I.Named I.Instruction]
-- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateIExpression iAssign
  | (IAssign (LValue (VarSymbol name Int)) (ILit val)) <- iAssign =
      return
        [ I.Do $ I.Store
                   False
                   (O.LocalReference T.i32 (AST.Name name))
                   (O.ConstantOperand $ i32Lit val)
                   Nothing
                   align4
                   defaultInstrMeta
        ]
  -- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (GlobalVarSymbol "M" Int))))
  | (IAssign (LValue (VarSymbol name Int)) (IVar (GlobalVarSymbol name2 _))) <- iAssign =
      return
        [ N.UnName 1 I.:= I.Load
                            False
                            (O.ConstantOperand (C.GlobalReference T.i32 (AST.Name name2)))
                            Nothing
                            align4
                            defaultInstrMeta
        , I.Do $ I.Store
                   False
                   (O.LocalReference T.i32 (AST.Name name))
                   (O.LocalReference T.i32 (N.UnName 1))
                   Nothing
                   align4
                   defaultInstrMeta
        ]
  -- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
  | (IAssign (LValue (VarSymbol name Int)) (IVar (VarSymbol name2 _))) <- iAssign =
      return
        [ N.UnName 1 I.:= I.Load False (O.LocalReference T.i32 (AST.Name name2)) Nothing align4
                            defaultInstrMeta
        , I.Do $ I.Store
                   False
                   (O.LocalReference T.i32 (AST.Name name))
                   --  (O.LocalReference T.i32 (AST.Name name2))
                   (O.LocalReference T.i32 (N.UnName 1))
                   Nothing
                   align4
                   defaultInstrMeta
        ]
  | otherwise = return [] -- TODO

-- TODO Generate all expressions
generateExpression :: Expr -> Codegen [I.Named I.Instruction]
generateExpression (IExpr expr) = generateIExpression expr

register :: Integer -> N.Name
register = N.UnName . fromInteger

passArg :: Expr -> Codegen ([I.Named I.Instruction], (O.Operand, [A.ParameterAttribute]))
passArg expr
  | (IExpr (IVar (VarSymbol name Int))) <- expr =
      do
        freeRegister <- currentRegisterNumber
        increaseRegisterNumber
        return
          ([ register freeRegister I.:= I.Load
                                          False
                                          (O.LocalReference T.i32 (AST.Name name))
                                          Nothing
                                          align4
                                          defaultInstrMeta
           ], (O.LocalReference T.i32 (register freeRegister), []))
  | (FExpr (FVar (VarSymbol name Real))) <- expr =
      do
        counter <- currentRegisterNumber
        increaseRegisterNumber
        return
          ([ N.UnName (fromInteger counter) I.:= I.Load
                                                   False
                                                   (O.LocalReference T.i32 (AST.Name name))
                                                   Nothing
                                                   align4
                                                   defaultInstrMeta
           ], (O.LocalReference T.float (N.UnName (fromInteger counter)), []))
  | (BExpr (BVar (VarSymbol name Bool))) <- expr =
      do
        counter <- currentRegisterNumber
        increaseRegisterNumber
        return
          ([ N.UnName (fromInteger counter) I.:= I.Load
                                                   False
                                                   (O.LocalReference T.i32 (AST.Name name))
                                                   Nothing
                                                   align4
                                                   defaultInstrMeta
           ], (O.LocalReference T.i8 (N.UnName (fromInteger counter)), []))
  -- SExpr (VarSymbol "format" String)
  | (SExpr (GlobalVarSymbol name String)) <- expr =
      return
        ([], (O.ConstantOperand
                (C.GetElementPtr True (C.GlobalReference strPointerType (AST.Name name))
                   [i32Lit 0, i32Lit 0]), []))

-- TODO Generate all statements
generateStatement :: Stmt -> Codegen [I.Named I.Instruction]
-- (VarLet (VarSymbol "b" Int) (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateStatement stmt
  | (VarLetStmt (VarLet (VarSymbol name Int) expr)) <- stmt =
      do
        allocInstr <- return (AST.Name name I.:= I.Alloca T.i32 Nothing align4 defaultInstrMeta)
        initInstrs <- generateExpression expr
        return $ allocInstr : initInstrs
  -- Expr (...)
  | (Expr e) <- stmt = generateExpression e
  -- VCall (FnSymbol "foo" Nothing) [args]
  | (VCall (FnSymbol name Nothing) args) <- stmt =
      do
        argsWithInstructions <- mapM passArg args
        instructions <- return $ concatMap fst argsWithInstructions
        call <- return
                  [ I.Do $ I.Call
                             Nothing
                             CC.C
                             []
                             (Right $ O.ConstantOperand $ C.GlobalReference
                                                            (T.FunctionType T.void [] False)
                                                            (N.Name name))
                             (map snd argsWithInstructions)
                             [Left $ A.GroupID 0]
                             defaultInstrMeta
                  ]
        return $ instructions ++ call

generateStatements :: [Stmt] -> Codegen [I.Named I.Instruction]
generateStatements stmts =
  do
    gens <- mapM generateStatement stmts
    return $ concat gens

-- TODO
generateReturnTerminator :: Stmt -> Codegen (I.Named I.Terminator)
generateReturnTerminator (Return e) = return $ globalGen e
  where
    genILit l = I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L415

                  (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                     --  (Just $ O.ConstantOperand $ i32Lit $ toInteger l)
                     (Just $ O.ConstantOperand $ i32Lit l)
                     defaultInstrMeta)
    genVoid = I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L415

                (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                   Nothing
                   defaultInstrMeta)
    globalGen (Just (IExpr (ILit l))) = genILit l
    globalGen Nothing = genVoid

generateReturnTerminator stmt = error
                                  ("'generateReturnTerminator' accepts only Return statements, '" ++
                                   show stmt ++
                                   "' given.")

makeBody :: SymbolToRegisterTable -> String -> [Stmt] -> Codegen [AST.BasicBlock]
makeBody table blockName statements =
  do
    stmts <- generateStatements $ init statements
    terminator <- generateReturnTerminator $ last statements
    return [block blockName stmts terminator]

constLetInAST :: ConstLet -> AST.Global
{-|
  ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
  ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
  ConstLet (ConstSymbol "format" String) (StringVal "test %d")
-}
constLetInAST constLet    -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

 =
  case constLet of
    (ConstLet (ConstSymbol s Int) (IntVal v))       -> gen T.i32 (enrich s) $ i32Lit $ toInteger v
    (ConstLet (ConstSymbol s Real) (RealVal v))     -> gen T.float (enrich s) $ f32Lit v
    (ConstLet (ConstSymbol s String) (StringVal v)) -> genStr (strArrayType (length v)) s $ strLit v
  where
    gen t n v = AST.GlobalVariable
                  (AST.Name n)
                  L.Internal
                  V.Default
                  Nothing
                  Nothing
                  defaultAddrSpace
                  True
                  True
                  t
                  (Just v)
                  Nothing
                  Nothing
                  align4
    genStr t n v = AST.GlobalVariable
                     (AST.Name n)
                     L.Private
                     V.Default
                     Nothing
                     Nothing
                     defaultAddrSpace
                     True
                     True
                     t
                     (Just v)
                     Nothing
                     Nothing
                     align1
    enrich = const "const"

constLetListInAST :: [ConstLet] -> [AST.Global]
constLetListInAST = map constLetInAST

staticVarLetInAST :: VarLet -> AST.Global
{-|
  VarLet (GlobalVarSymbol "M" Int) (IExpr (ILit 5))
  VarLet (GlobalVarSymbol "Moo" Real) (FExpr (FLit 55.5)
-}
staticVarLetInAST varLet =
  case varLet of
    (VarLet (GlobalVarSymbol s Int) (IExpr (ILit v)))  -> gen T.i32 s $ i32Lit $ toInteger v
    (VarLet (GlobalVarSymbol s Real) (FExpr (FLit v))) -> gen T.float s $ f32Lit v
  where
    gen t n v = AST.GlobalVariable  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

                  (AST.Name n)
                  L.Internal
                  V.Default
                  Nothing
                  Nothing
                  defaultAddrSpace
                  False
                  False
                  t
                  (Just v)
                  Nothing
                  Nothing
                  align4

staticVarLetListInAST :: [VarLet] -> [AST.Global]
staticVarLetListInAST = map staticVarLetInAST

formalArgs :: Symbol -> AST.Parameter
formalArgs (VarSymbol name Int) = AST.Parameter T.i32 (N.Name name) []
formalArgs (VarSymbol name Real) = AST.Parameter T.float (N.Name name) []
formalArgs (VarSymbol name Bool) = AST.Parameter T.i8 (N.Name name) []
formalArgs (VarSymbol _ String) = AST.Parameter strPointerType (N.Name "") []

fnLetInAST :: FnLet -> AST.Global
-- DeclareFnLet (FnSymbol "printf" Nothing) [VarSymbol "format" String] vararg
fnLetInAST (DeclareFnLet (FnSymbol name retType) arguments vararg) =
  case retType of
    Nothing  -> gen T.void name
    Just Int -> gen T.i32 name
  where
    gen t n = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                L.External
                V.Default
                Nothing
                CC.C
                []
                t
                (AST.Name n)
                (map formalArgs arguments, vararg)
                [Left $ A.GroupID 0]
                Nothing
                Nothing
                defaultAlignment
                Nothing
                Nothing
                []
                Nothing
-- FnLet (FnSymbol "name" Nothing) [args] [stmts]
fnLetInAST (FnLet (FnSymbol name retType) arguments statements) =
  case retType of
    Nothing -> gen T.void name $ evalState (makeBody table "entry-block" statements)
                                   initialCodegenState
    Just Int -> gen T.i32 name $ evalState (makeBody table "entry-block" statements)
                                   initialCodegenState
  where
    gen t n bs = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                   L.External
                   V.Default
                   Nothing
                   CC.C
                   []
                   t
                   (AST.Name n)
                   (map formalArgs arguments, False)
                   [Left $ A.GroupID 0]
                   Nothing
                   Nothing
                   defaultAlignment
                   Nothing
                   Nothing
                   (block "args-to-regs" (concat codeArgsToRegs)
                      (I.Do $ I.Br (AST.Name "entry-block") defaultInstrMeta) : bs)
                   Nothing
    argsToRegs = unzip $ zipWith argToReg arguments [1 ..]
    codeArgsToRegs = fst $ argsToRegs
    table = snd $ argsToRegs

argToReg :: Symbol -> Integer -> ([I.Named I.Instruction], (Symbol, N.Name))
argToReg symbol regNumber = (argToRegInstructions symbol (register regNumber), (symbol, (register
                                                                                           regNumber)))
  where
    argToRegInstructions sym reg
      | (VarSymbol name Int) <- sym =
          [ reg I.:= I.Alloca T.i32 Nothing align4 defaultInstrMeta
          , I.Do $ I.Store
                     False
                     (O.LocalReference T.i32 reg)
                     (O.LocalReference T.i32 (AST.Name name))
                     Nothing
                     align4
                     defaultInstrMeta
          ]
      | (VarSymbol name Real) <- sym =
          [ reg I.:= I.Alloca T.float Nothing align4 defaultInstrMeta
          , I.Do $ I.Store
                     False
                     (O.LocalReference T.float reg)
                     (O.LocalReference T.float (AST.Name name))
                     Nothing
                     align4
                     defaultInstrMeta
          ]
      | (VarSymbol name Bool) <- sym =
          [ reg I.:= I.Alloca T.i1 Nothing align4 defaultInstrMeta
          , I.Do $ I.Store
                     False
                     (O.LocalReference T.i1 reg)
                     (O.LocalReference T.i1 (AST.Name name))
                     Nothing
                     align4
                     defaultInstrMeta
          ]

fnLetListInAST :: [FnLet] -> [AST.Global]
fnLetListInAST = map fnLetInAST

-- TODO Move this to the parser
mainToPseudomain :: FnLet -> FnLet
mainToPseudomain (FnLet (FnSymbol _ ret) args statements) = FnLet (FnSymbol "falsum_main" ret) args
                                                              statements

-- TODO Move this to the parser
mainInAST :: FnLet -> [AST.Global]
mainInAST m = [ fnLetInAST $ mainToPseudomain m
              , fnLetInAST $ FnLet (FnSymbol "main" (Just Int)) []
                               [ VCall (FnSymbol "falsum_main" Nothing) []
                               , Return (Just (IExpr (ILit 0)))
                               ]
              ]

programInAST :: Program -> [AST.Global]
programInAST (Program constLetList staticVarLetList fnLetList mainLet) = staticVarLetListInAST
                                                                           staticVarLetList ++
                                                                         constLetListInAST
                                                                           constLetList ++
                                                                         fnLetListInAST fnLetList ++
                                                                         mainInAST mainLet

defaultfunctionAttributes :: AST.Definition
defaultfunctionAttributes = AST.FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.UWTable]

topLevelDefs :: Program -> [AST.Definition]
topLevelDefs program = [defaultfunctionAttributes] ++ fmap AST.GlobalDefinition
                                                        (programInAST program)

{-|
  TODO Set filename as module name
  TODO(optional) Set module DataLayout
  TODO(optional) Set module TargetTriple
-}
moduleInAST :: Program -> AST.Module
moduleInAST program = highLevelModule `debug` PP.showPretty highLevelModule
  where
    highLevelModule = AST.Module "01_simple" Nothing Nothing $ topLevelDefs program

{-main =
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            ast <- LLVMMod.moduleAST mod
            putStrLn $ LLVMPP.showPretty ast
            liftError $ LLVMMod.withModuleFromAST ctx ast$ \mod -> do
                putStrLn "Success!"

    where
        file = LLVMMod.File "Rx-linked-cg.bc"-}
asm :: Program -> IO String
asm program = CTX.withContext $ \ctx ->
  liftError $ MOD.withModuleFromAST ctx (moduleInAST program) MOD.moduleLLVMAssembly

main :: IO ()
main = do
  --putStrLn $ show problem
  llvmIR <- asm simple
  putStrLn llvmIR
