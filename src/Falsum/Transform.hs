module Falsum.Transform where

import           Falsum.AST
import           System.Random

{-
simple :: Program
simple = Program [] [] []
           (FnLet (FnSymbol "main" Nothing) []
              [VarLetStmt (VarLet (VarSymbol "test" Int) (IExpr (ILit 42)))])
-}
lowLevelMain :: String -> FnLet
lowLevelMain highLevelMain = FnLet (FnSymbol "main" [] (Just Int)) []
                               [ VCall (FnSymbol highLevelMain [] Nothing) []
                               , Return (Just (IExpr (ILit 0)))
                               ]

randomString :: Int -> IO String
randomString len = do
  g <- getStdGen
  return $ take len (randomRs ('a', 'z') g)

transformProgram :: String -> Program -> Program
transformProgram newMain (Program consts vars fns mainFn) =
  Program consts vars (map (transformFn newMain) fns ++ [transformFn newMain mainFn])
    (lowLevelMain newMain)

transformSymbol :: String -> Symbol -> Symbol
transformSymbol newMain (FnSymbol "main" [] ty) = FnSymbol newMain [] ty
transformSymbol _ s = s

transformStmt :: String -> Stmt -> Stmt
transformStmt newMain stmt =
  case stmt of
    Loop stmts            -> Loop $ map (transformStmt newMain) stmts
    While condition stmts -> While condition $ map (transformStmt newMain) stmts
    VCall sym args        -> VCall (transformSymbol newMain sym) args
    anything              -> anything

transformFn :: String -> FnLet -> FnLet
transformFn newMain (FnLet sym args body) = FnLet
                                              (transformSymbol newMain sym)
                                              (map (transformSymbol newMain) args)
                                              (map (transformStmt newMain) body)
