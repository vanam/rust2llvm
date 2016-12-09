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
-- , TODO the rest of program, but AST.hs is missing the assignment expression for now
{-
// Binary Operations
r = a + b;
r = b - c;
r = c * d;
r = d / a;
r = d % c;

// Bitwise Binary Operations
r = a & b;
r = a & b;
r = b | c;
r = c ^ d;
r = !a;
r = a << b;
r = a >> b;
-}
   ])
