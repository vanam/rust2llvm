module Test_01_Simple where

import Falsum.AST

simple :: Program
simple = Program
              [ ConstLet (ConstSymbol "ANSWER" Int) (IntVal 42)
              ]
              [ VarLet (GlobalVarSymbol "M" Int) (IExpr (ILit 5))
              ]
              [ FnLet (FnSymbol "foo" Nothing) []
                  [VarLetStmt (VarLet (VarSymbol "a" Int) (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 21)))), Return Nothing]
              ]
              (FnLet (FnSymbol "main" Nothing) []
                 [ VarLetStmt
                     (VarLet (VarSymbol "a" Int)
                        (IExpr (IAssign (LValue (VarSymbol "a" Int)) (IVar (GlobalVarSymbol "M" Int)))))

                 , VarLetStmt
                     (VarLet (VarSymbol "b" Int)
                        (IExpr (IAssign (LValue (VarSymbol "a" Int)) (ILit 42))))
                 ,
                 Return Nothing
                 ]
               )
