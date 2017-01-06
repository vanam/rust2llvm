module Falsum.Codegen where

--import Control.Monad hiding (void)
import           Data.Char
import           Data.Word
import           Debug.Trace
import qualified Falsum.AST                         as F
import           Falsum.Lexer                       (backwardLookup,
                                                     forwardLookup)
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
import           LLVM.General.AST.Constant
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
debug = flip trace

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

simple :: F.Program
simple = F.Program
           [ F.ConstLet (F.ConstSymbol "ANSWER" F.Int) (F.IntVal 42)
           , F.ConstLet (F.ConstSymbol "ANSWERE" F.Real) (F.RealVal 420)
           , F.ConstLet (F.ConstSymbol "format" F.String) (F.StringVal "test %d\00")
           ]
           [ F.VarLet (F.GlobalVarSymbol "M" F.Int) (F.IExpr (F.ILit 5))
           , F.VarLet (F.GlobalVarSymbol "Moo" F.Real) (F.FExpr (F.FLit 55.5))
           ]
           [ F.FnLet (F.FnSymbol "foo" Nothing) []
               [ F.VarLetStmt
                   (F.VarLet (F.VarSymbol "a" F.Int)
                      (F.IExpr (F.IAssign (F.LValue (F.VarSymbol "a" F.Int)) (F.ILit 21))))
               , F.Return Nothing
               ]
           , F.FnLet
               (F.FnSymbol "maine" (Just F.Int))
               [F.VarSymbol "a" F.Int, F.VarSymbol "b" F.Int]
               [F.VCall (F.FnSymbol "foo" Nothing) [], F.Return (Just (F.IExpr (F.ILit 0)))]
           , F.DeclareFnLet (F.FnSymbol "printf" (Just F.Int)) [F.VarSymbol "" F.String] True
           ]
           (F.FnLet (F.FnSymbol "main" Nothing) []
              [ F.VarLetStmt
                  (F.VarLet (F.VarSymbol "a" F.Int)
                     (F.IExpr
                        (F.IAssign (F.LValue (F.VarSymbol "a" F.Int))
                           (F.IVar (F.GlobalVarSymbol "M" F.Int)))))
              , F.VarLetStmt
                  (F.VarLet (F.VarSymbol "b" F.Int)
                     (F.IExpr (F.IAssign (F.LValue (F.VarSymbol "a" F.Int)) (F.ILit 42))))
              , F.Expr
                  (F.IExpr
                     (F.IAssign (F.LValue (F.VarSymbol "a" F.Int)) (F.IVar (F.VarSymbol "b" F.Int))))
              , F.Expr
                  (F.IExpr
                     (F.IAssign (F.LValue (F.VarSymbol "a" F.Int)) (F.IVar (F.VarSymbol "b" F.Int))))
              , F.Expr
                  (F.IExpr
                     (F.IAssign (F.LValue (F.VarSymbol "a" F.Int)) (F.IVar (F.VarSymbol "b" F.Int))))
              , F.VCall (F.FnSymbol "foo" Nothing) []
              , F.VCall (F.FnSymbol "maine" Nothing)
                  [ F.IExpr (F.IVar (F.VarSymbol "a" F.Int))
                  , F.IExpr (F.IVar (F.VarSymbol "b" F.Int))
                  ]
              , F.VCall (F.FnSymbol "printf" Nothing)
                  [ F.SExpr (F.GlobalVarSymbol "format" F.String)
                  , F.IExpr (F.IVar (F.VarSymbol "b" F.Int))
                  ]
              , F.Return Nothing
              ])

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
generateIExpression iAssign
  | (F.IAssign (F.LValue (F.VarSymbol name F.Int)) (F.ILit val)) <- iAssign =
      return
        [ Do $ Store
                 False
                 (LocalReference i32 (Name name))
                 (ConstantOperand $ i32Lit val)
                 Nothing
                 align4
                 defaultInstrMeta
        ]
  -- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (GlobalVarSymbol "M" Int))))
  | (F.IAssign (F.LValue (F.VarSymbol name F.Int)) (F.IVar (F.GlobalVarSymbol name2 _))) <- iAssign =
      return
        [ UnName 1 := Load False (ConstantOperand (GlobalReference i32 (Name name2))) Nothing align4
                        defaultInstrMeta
        , Do $ Store
                 False
                 (LocalReference i32 (Name name))
                 (LocalReference i32 (UnName 1))
                 Nothing
                 align4
                 defaultInstrMeta
        ]
  -- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
  | (F.IAssign (F.LValue (F.VarSymbol name F.Int)) (F.IVar (F.VarSymbol name2 _))) <- iAssign =
      return
        [ UnName 1 := Load False (LocalReference i32 (Name name2)) Nothing align4 defaultInstrMeta
        , Do $ Store
                 False
                 (LocalReference i32 (Name name))
                 --  (O.LocalReference T.i32 (AST.Name name2))
                 (LocalReference i32 (UnName 1))
                 Nothing
                 align4
                 defaultInstrMeta
        ]
  | otherwise = return [] -- TODO

-- TODO Generate all expressions
generateExpression :: F.Expr -> Codegen [Named Instruction]
generateExpression (F.IExpr expr) = generateIExpression expr

passArg :: F.Expr -> Codegen ([Named Instruction], (Operand, [ParameterAttribute]))
passArg expr
  | (F.IExpr (F.IVar (F.VarSymbol name F.Int))) <- expr = genPassing name i32
  | (F.FExpr (F.FVar (F.VarSymbol name F.Real))) <- expr = genPassing name float
  | (F.BExpr (F.BVar (F.VarSymbol name F.Bool))) <- expr = genPassing name i1
  -- SExpr (VarSymbol "format" String)
  | (F.SExpr (F.GlobalVarSymbol name F.String)) <- expr =
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
  | (F.VCall (F.FnSymbol name Nothing) args) <- stmt =
      do
        argsWithInstructions <- mapM passArg args
        instructions <- return $ concatMap fst argsWithInstructions
        call <- return
                  [ Do $ Call
                           Nothing
                           C
                           []
                           (Right $ ConstantOperand $ GlobalReference (FunctionType void [] False)
                                                        (Name name))
                           (map snd argsWithInstructions)
                           [Left $ GroupID 0]
                           defaultInstrMeta
                  ]
        return $ instructions ++ call

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
stmtsInAST table blockName statements =
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
    enrich = const "const"

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
staticVarLetInAST varLet =
  case varLet of
    (F.VarLet (F.GlobalVarSymbol s F.Int) (F.IExpr (F.ILit v))) -> genGVar False s i32 $ i32Lit $ toInteger
                                                                                                    v
    (F.VarLet (F.GlobalVarSymbol s F.Real) (F.FExpr (F.FLit v))) -> genGVar False s float $ f32Lit v

staticVarLetListInAST :: [F.VarLet] -> [Global]
staticVarLetListInAST = map staticVarLetInAST

formalArgs :: F.Symbol -> Parameter
formalArgs (F.VarSymbol name ty) = Parameter (genTy . Just $ ty) (Name name) []

genTy :: (Maybe F.ValueType) -> Type
genTy ty =
  case ty of
    Nothing       -> void
    Just F.Int    -> i32
    Just F.Real   -> float
    Just F.Bool   -> i1
    Just F.String -> strPointerType

genFn :: Type -> String -> [F.Symbol] -> Bool -> [BasicBlock] -> Global
genFn ty name args isVararg body = Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                                     External
                                     Default
                                     Nothing
                                     C
                                     []
                                     ty
                                     (Name name)
                                     (map formalArgs args, isVararg)
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
fnLetInAST (F.DeclareFnLet (F.FnSymbol name retType) args vararg) =
  case retType of
    Nothing    -> genFn void name args vararg []
    Just F.Int -> genFn i32 name args vararg []
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
fnLetInAST' (F.FnLet (F.FnSymbol name retType) args statements) =
  do
    argsToRegs <- unzip <$> mapM argToReg args
    codeArgsToRegs <- return $ fst argsToRegs
    table <- return $ snd argsToRegs
    prologueBlock <- withSimpleTerminator (concat codeArgsToRegs)
    increaseBlockIdentifier
    entryBlockId <- currentBlockIdentifier
    bodyBlocks <- stmtsInAST table entryBlockId statements
    return $ genFn (genTy retType) name args False $ prologueBlock : bodyBlocks

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

-- TODO Move this to the parser
mainToPseudomain :: F.FnLet -> F.FnLet
mainToPseudomain (F.FnLet (F.FnSymbol _ ret) args statements) = F.FnLet
                                                                  (F.FnSymbol "falsum_main" ret)
                                                                  args
                                                                  statements

-- TODO Move this to the parser
mainInAST :: F.FnLet -> [Global]
mainInAST m = [ fnLetInAST $ mainToPseudomain m
              , fnLetInAST $ F.FnLet (F.FnSymbol "main" (Just F.Int)) []
                               [ F.VCall (F.FnSymbol "falsum_main" Nothing) []
                               , F.Return (Just (F.IExpr (F.ILit 0)))
                               ]
              ]

programInAST :: F.Program -> [Global]
programInAST (F.Program constLetList staticVarLetList fnLetList mainLet) = staticVarLetListInAST
                                                                             staticVarLetList ++
                                                                           constLetListInAST
                                                                             constLetList ++
                                                                           fnLetListInAST fnLetList ++
                                                                           mainInAST mainLet

defaultfunctionAttributes :: Definition
defaultfunctionAttributes = FunctionAttributes (GroupID 0) [NoUnwind, UWTable]

topLevelDefs :: F.Program -> [Definition]
topLevelDefs program = [defaultfunctionAttributes] ++ fmap GlobalDefinition (programInAST program)

{-|
  TODO Set filename as module name
  TODO(optional) Set module DataLayout
  TODO(optional) Set module TargetTriple
-}
moduleInAST :: F.Program -> Module
moduleInAST program = highLevelModule `debug` showPretty highLevelModule
  where
    highLevelModule = Module "01_simple" Nothing Nothing $ topLevelDefs program

{-main =
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            ast <- LLVMMod.moduleAST mod
            putStrLn $ LLVMPP.showPretty ast
            liftError $ LLVMMod.withModuleFromAST ctx ast$ \mod -> do
                putStrLn "Success!"

    where
        file = LLVMMod.File "Rx-linked-cg.bc"-}
asm :: F.Program -> IO String
asm program = withContext $ \ctx ->
  liftError $ withModuleFromAST ctx (moduleInAST program) moduleLLVMAssembly

main :: IO ()
main = do
  --putStrLn $ show problem
  llvmIR <- asm simple
  putStrLn llvmIR
