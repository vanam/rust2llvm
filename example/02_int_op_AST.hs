module Test_02_int_op where

import Falsum.AST

int_op :: Program
int_op = Program [] [] []
  (FnLet (FnSymbol "main" [] Nothing) []
   [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (ILit 5)))
   ,VarLetStmt (VarLet (VarSymbol "b" Int) (IExpr (ILit 7)))
   ,VarLetStmt (VarLet (VarSymbol "c" Int) (IExpr (ILit 9)))
   ,VarLetStmt (VarLet (VarSymbol "d" Int) (IExpr (ILit 11)))
   ,VarLetStmt (VarLet (VarSymbol "r" Int) (IExpr (ILit 0)))  -- are we supporting non-initialized variables?
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IPlus (IVar (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IMinus (IVar (VarSymbol "b" Int)) (IVar (VarSymbol "c" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IMult (IVar (VarSymbol "c" Int)) (IVar (VarSymbol "d" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IDiv (IVar (VarSymbol "d" Int)) (IVar (VarSymbol "a" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IMod (IVar (VarSymbol "d" Int)) (IVar (VarSymbol "c" Int)))))

   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IAnd (IVar (VarSymbol "a" Int)) (IVar (VarSymbol "b" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IOr (IVar (VarSymbol "b" Int)) (IVar (VarSymbol "c" Int)))))
   ,Expr (IExpr (IAssign (LValue (VarSymbol "r" Int)) (IBinary IXor (IVar (VarSymbol "c" Int)) (IVar (VarSymbol "d" Int)))))
-- r = !a; Nope we are not doing that
-- TODO shift operators are missing in AST.hs
{-
// Bitwise Binary Operations
r = a << b;
r = a >> b;
-}
   ])
