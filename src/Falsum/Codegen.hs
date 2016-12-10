module Falsum.Codegen where

import           Data.Word
import           Falsum.AST
import qualified LLVM.General.AST                        as AST
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Instruction            as I
import qualified LLVM.General.AST.Linkage                as L
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

defaultAlignment :: Word32
defaultAlignment = 4

i32Lit :: Integer -> C.Constant
i32Lit = C.Int 32

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
                  defaultAlignment

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
             (Just $ i32Lit 42)
             Nothing
             Nothing
             defaultAlignment

body :: [I.Named I.Instruction] -> (I.Named I.Terminator) -> [AST.BasicBlock]
body instructions terminator = [AST.BasicBlock -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Global.hs#L74

                                  (AST.Name "")
                                  instructions
                                  terminator]

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
               []
               Nothing
               Nothing
               defaultAlignment
               Nothing
               Nothing
               (body []
                  (I.Do -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L415

                     (I.Ret -- https://github.com/bscarlet/llvm-general/blob/llvm-3.5/llvm-general-pure/src/LLVM/General/AST/Instruction.hs#L24

                        Nothing
                        [])))

-- TODO
simple_main = undefined

topLevelDefs :: [AST.Definition]
topLevelDefs = fmap AST.GlobalDefinition $
  [simple_ANSWER, simple_M, simple_foo]

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
