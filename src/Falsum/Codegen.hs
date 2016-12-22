module Falsum.Codegen where

import           Data.Word
import           Debug.Trace
import           Falsum.AST
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

debug :: a -> String -> a
debug = flip trace

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

simple :: Program
simple = Program
           [ ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
           , ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
           ]
           [ VarLet (GlobalVarSymbol "M" Int) (IExpr (ILit 5))
           , VarLet (GlobalVarSymbol "Moo" Real) (FExpr (FLit 55.5))
           ]
           [ FnLet (FnSymbol "foo" Nothing) []
               [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (ILit 21))), Return Nothing]-- return
                                                                                          -- void
           , FnLet (FnSymbol "maine" (Just Int)) []
               [VCall (FnSymbol "foo" Nothing) [], Return (Just (IExpr (ILit 0)))]
           ]
           (FnLet (FnSymbol "main" Nothing) []
              [ VarLetStmt
                  (VarLet (VarSymbol "a" Int)
                     (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (GlobalVarSymbol "M" Int))))) -- everything is mutable

              , VarLetStmt
                  (VarLet (VarSymbol "b" Int)
                     (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42)))) -- ANSWER value placed
              , Expr (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
              , VCall (FnSymbol "foo" Nothing) []
              ,
              -- directly here
              Return Nothing
              ]-- return void
            )

defaultInstrMeta :: I.InstructionMetadata
defaultInstrMeta = []

defaultAlignment :: Word32
defaultAlignment = 0

align4 :: Word32
align4 = 4

i32Lit :: Integer -> C.Constant
i32Lit = C.Int 32

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
generateIExpression :: IExpr -> [I.Named I.Instruction]
-- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateIExpression iAssign
  | (IAssign (LValue (VarSymbol name Int)) (ILit val)) <- iAssign =
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
      [ I.Do $ I.Store
                 False
                 (O.LocalReference T.i32 (AST.Name name))
                 (O.ConstantOperand (C.GlobalReference T.i32 (AST.Name name2)))
                 Nothing
                 align4
                 defaultInstrMeta
      ]
  -- (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int))))
  | (IAssign (LValue (VarSymbol name Int)) (IVar (VarSymbol name2 _))) <- iAssign =
      [ I.Do $ I.Store
                 False
                 (O.LocalReference T.i32 (AST.Name name))
                 (O.LocalReference T.i32 (AST.Name name2))
                 Nothing
                 align4
                 defaultInstrMeta
      ]
  | otherwise = [] -- TODO

-- TODO Generate all expressions
generateExpression :: Expr -> [I.Named I.Instruction]
generateExpression (IExpr expr) = generateIExpression expr

-- TODO Generate all statements
generateStatement :: Stmt -> [I.Named I.Instruction]
-- (VarLet (VarSymbol "b" Int) (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
generateStatement stmt
  | (VarLetStmt (VarLet (VarSymbol name Int) expr)) <- stmt =
      (AST.Name name I.:= I.Alloca T.i32 Nothing align4 defaultInstrMeta) : generateExpression expr
  -- Expr (...)
  | (Expr e) <- stmt = generateExpression e
  -- VCall (FnSymbol "foo" Nothing) [] -- TODO Pass arguments
  | (VCall (FnSymbol name Nothing) _) <- stmt =
      [ I.Do $ I.Call
                 Nothing
                 CC.C
                 []
                 (Right $ O.ConstantOperand $ C.GlobalReference (T.FunctionType T.void [] False)
                                                (N.Name name))
                 []
                 [Left $ A.GroupID 0]
                 defaultInstrMeta
      ]

generateStatements :: [Stmt] -> [I.Named I.Instruction]
generateStatements = concatMap generateStatement

-- TODO
generateReturnTerminator :: Stmt -> I.Named I.Terminator
generateReturnTerminator (Return e) = globalGen e
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

makeBody :: String -> [Stmt] -> [AST.BasicBlock]
makeBody blockName statements = [ block blockName (generateStatements $ init statements)
                                    (generateReturnTerminator $ last statements)
                                ]

-- TODO
simple_foo :: AST.Global
simple_foo = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

               L.Internal
               V.Default
               Nothing
               CC.C
               []
               T.void
               (AST.Name "foo")
               ([], False)
               [Left $ A.GroupID 0]
               Nothing
               Nothing
               defaultAlignment
               Nothing
               Nothing
               (body
                  "entry-block"
                  [ (AST.Name "a") I.:= I.Alloca T.i32 Nothing align4 defaultInstrMeta
                  , I.Do $ I.Store
                             False
                             (O.LocalReference T.i32 (AST.Name "a"))
                             (O.ConstantOperand $ i32Lit 21)
                             Nothing
                             align4
                             defaultInstrMeta
                  ]
                  (I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L417

                     (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                        Nothing
                        defaultInstrMeta)))
               Nothing

-- TODO
simple_main :: AST.Global
simple_main = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                L.External
                V.Default
                Nothing
                CC.C
                []
                T.void
                (AST.Name "main")
                ([], False)
                [Left $ A.GroupID 0]
                Nothing
                Nothing
                defaultAlignment
                Nothing
                Nothing
                (body
                   "entry-block"
                   [ (AST.Name "a") I.:= I.Alloca T.i32 Nothing defaultAlignment defaultInstrMeta
                   , (AST.Name "b") I.:= I.Alloca T.i32 Nothing defaultAlignment defaultInstrMeta
                   , I.Do $ I.Store
                              False
                              (O.LocalReference T.i32 (AST.Name "a"))
                              (O.ConstantOperand $ i32Lit 5)
                              Nothing
                              align4
                              defaultInstrMeta
                   , I.Do $ I.Store
                              False
                              (O.LocalReference T.i32 (AST.Name "b"))
                              (O.ConstantOperand $ i32Lit 42)
                              Nothing
                              align4
                              defaultInstrMeta
                   ]
                   (I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L417

                      (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                         Nothing
                         defaultInstrMeta)))
                Nothing

constLetInAST :: ConstLet -> AST.Global
{-|
  ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
  ConstLet (ConstSymbol "ANSWERE" Real) (RealVal 420)
-}
constLetInAST constLet    -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

 =
  case constLet of
    (ConstLet (ConstSymbol s Int) (IntVal v))   -> gen T.i32 (enrich s) $ i32Lit $ toInteger v
    (ConstLet (ConstSymbol s Real) (RealVal v)) -> gen T.float (enrich s) $ f32Lit v
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

-- FnLet (FnSymbol "name" Nothing) [...] [...]
fnLetInAST :: FnLet -> AST.Global
fnLetInAST (FnLet (FnSymbol name retType) _ statements) =
  case retType of
    Nothing  -> gen T.void name $ makeBody "entry-block" statements
    Just Int -> gen T.i32 name $ makeBody "entry-block" statements
  where
    gen t n bs = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.8/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                   L.External
                   V.Default
                   Nothing
                   CC.C
                   []
                   t
                   (AST.Name n)
                   ([], False)
                   [Left $ A.GroupID 0]
                   Nothing
                   Nothing
                   defaultAlignment
                   Nothing
                   Nothing
                   bs
                   Nothing

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
