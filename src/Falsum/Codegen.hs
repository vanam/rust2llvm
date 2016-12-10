module Falsum.Codegen where

import           Falsum.AST
import qualified LLVM.General.AST                        as AST
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
           [ConstLet (ConstSymbol "ANSWER" Int (IntVal 42)) (IExpr (ILit 42))]
           [VarLet (VarSymbol "M" Int) (IExpr (ILit 5))]
           [ FnLet (FnSymbol "foo" [] Nothing) []
               [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (ILit 21)))]
           ]
           (FnLet (FnSymbol "main" [] Nothing) []
              [ VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (IVar (VarSymbol "M" Int)))) -- everything is mutable

              , VarLetStmt
                  (VarLet (VarSymbol "b" Int) (IExpr (IVar (ConstSymbol "ANSWER" Int (IntVal 42)))))
              ])

moduleInAST :: AST.Module
moduleInAST = AST.defaultModule

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
