module Test_01_Simple where

import Falsum.AST

simple :: Program
simple = Program
  [ConstLet (ConstSymbol "ANSWER" Int (IntVal 42)) (IExpr (ILit 42))]
  [VarLet (VarSymbol "M" Int) (IExpr (ILit 5))]
  [FnLet (FnSymbol "foo" [] Nothing) []
   [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (ILit 21)))]]
  (FnLet (FnSymbol "main" [] Nothing) []
   [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (IVar (VarSymbol "M" Int)))) -- everything is mutable
   ,VarLetStmt (VarLet (VarSymbol "b" Int) (IExpr (IVar (ConstSymbol "ANSWER" Int (IntVal 42)))))])
