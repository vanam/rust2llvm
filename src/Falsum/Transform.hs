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
transformProgram rndStr (Program consts vars fns mainFn) =
  Program consts vars (map (transformFn rndStr) fns ++ [transformFn rndStr mainFn])
    (lowLevelMain $ newMain rndStr)

newMain :: String -> String
newMain s = "main_" ++ s

transformSymbol :: String -> Symbol -> Symbol
transformSymbol rndStr (FnSymbol "main" [] ty) = FnSymbol (newMain rndStr) [] ty
transformSymbol _ s = s

transformStmt :: String -> Stmt -> Stmt
transformStmt rndStr stmt =
  case stmt of
    Loop stmts            -> Loop $ map (transformStmt rndStr) stmts
    While condition stmts -> While condition $ map (transformStmt rndStr) stmts
    VCall sym args        -> VCall (transformSymbol rndStr sym) args
    anything              -> anything

transformFn :: String -> FnLet -> FnLet
transformFn rndStr (FnLet sym args body) = FnLet
                                             (transformSymbol rndStr sym)
                                             (map (transformSymbol rndStr) args)
                                             (map (transformStmt rndStr) body)
