module Falsum.Codegen where

import           Data.Word
import           Falsum.AST
import qualified LLVM.General.AST                        as AST
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Attribute              as A
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Instruction            as I
import qualified LLVM.General.AST.Linkage                as L
import qualified LLVM.General.AST.Operand                as O
import qualified LLVM.General.AST.Type                   as T
import qualified LLVM.General.AST.Visibility             as V
-- import qualified LLVM.General.AST.DLL as DLL import qualified LLVM.General.AST.ThreadLocalStorage
-- as TLS
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.Context                    as CTX
import qualified LLVM.General.Module                     as MOD
import qualified LLVM.General.PrettyPrint                as PP

import           Control.Monad
import           Control.Monad.Trans.Except

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

simple :: Program
simple = Program
           [ConstLet (ConstSymbol "ANSWER" Int (IntVal 42)) (IExpr (ILit 42)),
           ConstLet (ConstSymbol "ANSWERE" Real (RealVal 420)) (FExpr (FLit 420))]
           [VarLet (VarSymbol "M" Int) (IExpr (ILit 5))]
           [ FnLet (FnSymbol "foo" [] Nothing) []
               [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (ILit 21)))]
           ]
           (FnLet (FnSymbol "main" [] Nothing) []
              [ VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (IVar (VarSymbol "M" Int)))) -- everything is mutable

              , VarLetStmt
                  (VarLet (VarSymbol "b" Int) (IExpr (IVar (ConstSymbol "ANSWER" Int (IntVal 42)))))
              ])

defaultInstrMeta :: I.InstructionMetadata
defaultInstrMeta = []

defaultAlignment :: Word32
defaultAlignment = 0

align4 :: Word32
align4 = 4

i32Lit :: Integer -> C.Constant
i32Lit = C.Int 32

f32Lit :: Float -> C.Constant
f32Lit f = C.Float $ F.Single f

defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

simple_ANSWER = AST.GlobalVariable -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

                  (AST.Name "ANSWER")
                  L.Internal
                  V.Default
                  Nothing
                  Nothing
                  defaultAddrSpace
                  True
                  True
                  T.i32
                  (Just $ i32Lit 42)
                  Nothing
                  Nothing
                  align4

simple_M = AST.GlobalVariable -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

             (AST.Name "M")
             L.Internal
             V.Default
             Nothing
             Nothing
             defaultAddrSpace
             False
             False
             T.i32
             (Just $ i32Lit 5)
             Nothing
             Nothing
             align4

block :: String -> [I.Named I.Instruction] -> (I.Named I.Terminator) -> AST.BasicBlock
block name instructions terminator = AST.BasicBlock  -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L74

                                       (AST.Name name)
                                       instructions
                                       terminator

body :: [I.Named I.Instruction] -> (I.Named I.Terminator) -> [AST.BasicBlock]
body instructions terminator = [block "" instructions terminator]

-- TODO
simple_foo :: AST.Global
simple_foo = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

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
                  [ (AST.Name "a") I.:= I.Alloca T.i32 Nothing align4 defaultInstrMeta
                  , I.Do $ I.Store
                             False
                             (O.LocalReference T.i32 (AST.Name "a"))
                             (O.ConstantOperand $ i32Lit 21)
                             Nothing
                             align4
                             defaultInstrMeta
                  ]
                  (I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L415

                     (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                        Nothing
                        defaultInstrMeta)))

-- TODO
simple_main :: AST.Global
simple_main = AST.Function -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L48

                L.Internal
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
                   (I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L415

                      (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                         Nothing
                         defaultInstrMeta)))

-- topLevelDefs :: [AST.Definition]
-- topLevelDefs = [AST.FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.UWTable]] ++ (fmap
--                                                                                     AST.GlobalDefinition $
--                                                                                     [ simple_ANSWER
--                                                                                     , simple_M
--                                                                                     , simple_foo
--                                                                                     , simple_main
--                                                                                     ])

--------------------------------------------------------------------------------
-- TODO Major refactoring needed ;-)
--------------------------------------------------------------------------------

-- TODO Add global counter for generating names for constants eg @const2778, @const2779 ...
constLetInAST :: ConstLet -> AST.Global
--[ConstLet (ConstSymbol "ANSWER" Int (IntVal 42)) (IExpr (ILit 42))]
constLetInAST cl@(ConstLet (ConstSymbol s Int (IntVal v)) _) = AST.GlobalVariable -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

                  (AST.Name s)
                  L.Internal
                  V.Default
                  Nothing
                  Nothing
                  defaultAddrSpace
                  True
                  True
                  T.i32
                  (Just $ i32Lit (toInteger v))
                  Nothing
                  Nothing
                  align4
constLetInAST cl@(ConstLet (ConstSymbol s Real (RealVal v)) _) = AST.GlobalVariable -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L21

                  (AST.Name s)
                  L.Internal
                  V.Default
                  Nothing
                  Nothing
                  defaultAddrSpace
                  True
                  True
                  T.float
                  (Just $ f32Lit ( v))
                  -- (v)
                  Nothing
                  Nothing
                  align4

constLetListInAST :: [ConstLet] -> [AST.Global]
constLetListInAST = map constLetInAST

varLetInAST :: VarLet -> AST.Global
varLetInAST l = simple_M-- TODO change to something meaningful

varLetListInAST :: [VarLet] -> [AST.Global]
varLetListInAST = map varLetInAST

fnLetInAST :: FnLet -> AST.Global
fnLetInAST l = simple_main-- TODO change to something meaningful

fnLetListInAST :: [FnLet] -> [AST.Global]
fnLetListInAST = map fnLetInAST

programInAST :: Program -> [AST.Global]
programInAST program@(Program constLetList varLetList fnLetList main) = constLetListInAST constLetList ++ varLetListInAST varLetList ++ fnLetListInAST fnLetList ++ [fnLetInAST main]

topLevelDefs :: [AST.Definition]
topLevelDefs = [AST.FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.UWTable]] ++ (fmap
                                                                                    AST.GlobalDefinition $
                                                                                    programInAST simple)-- TODO change 'simple' to Program passed as parameter
-- TODO Set filename as module name
-- TODO(optional) Set module DataLayout
-- TODO(optional) Set module TargetTriple
moduleInAST :: AST.Module
moduleInAST = AST.Module "01_simple" Nothing Nothing topLevelDefs

{-main =
    LLVMCtx.withContext $ \ctx ->
        liftError $ LLVMMod.withModuleFromBitcode ctx file $ \mod -> do
            ast <- LLVMMod.moduleAST mod
            putStrLn $ LLVMPP.showPretty ast
            liftError $ LLVMMod.withModuleFromAST ctx ast$ \mod -> do
                putStrLn "Success!"

    where
        file = LLVMMod.File "Rx-linked-cg.bc"-}
asm :: IO String
asm = CTX.withContext $ \ctx -> liftError $ MOD.withModuleFromAST ctx moduleInAST $ \moduleInLowLevel -> do
  MOD.moduleLLVMAssembly moduleInLowLevel

main = do
  llvmIR <- asm
  putStrLn llvmIR
