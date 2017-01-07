module Falsum.Transform where

import           Control.Monad.Trans.State.Lazy
import           Falsum.AST
import           System.Random

data TransformState = TransformState { declarations :: [ConstLet] }
  deriving Show

initialTransformState :: TransformState
initialTransformState = TransformState []

type Transform = State TransformState

lowLevelMain :: String -> FnLet
lowLevelMain highLevelMain = FnLet (FnSymbol "main" [] (Just Int)) []
                               [ VCall (FnSymbol highLevelMain [] Nothing) []
                               , Return (Just (IExpr (ILit 0)))
                               ]

declaration :: String -> String -> ConstLet
declaration name val = ConstLet (ConstSymbol name String) (StringVal val)

randomString :: Int -> IO String
randomString len = do
  g <- getStdGen
  return $ take len (randomRs ('a', 'z') g)

uniqueIdentifer :: String -> Transform String
uniqueIdentifer rndStr =
  do
    decls <- get
    return $ "format_" ++ rndStr ++ "_" ++ (show . length . declarations $ decls)

modifyTransform :: ([ConstLet] -> [ConstLet]) -> Transform ()
modifyTransform f =
  do
    current <- get
    put $ TransformState (f . declarations $ current)

externalFns :: [FnLet]
externalFns = [DeclareFnLet (VariadicFnSymbol "printf" [String] Nothing)]

transformProgram :: String -> Program -> Program
transformProgram rndStr (Program consts vars fns mainFn) =
  Program (consts ++ (declarations . snd $ tFnsAndDecls)) vars ((fst tFnsAndDecls) ++ externalFns)
    (lowLevelMain $ newMain rndStr)
  where
    tFnsAndDecls = runState (mapM (transformFn rndStr) (fns ++ [mainFn])) initialTransformState

newMain :: String -> String
newMain s = "main_" ++ s

transformSymbol :: String -> Symbol -> Symbol
transformSymbol rndStr (FnSymbol "main" [] ty) = FnSymbol (newMain rndStr) [] ty
transformSymbol _ s = s

transformExpr :: String -> Expr -> Transform Expr
transformExpr rndStr (SExpr (ConstSymbol stringAsName String)) = do
  newName <- uniqueIdentifer rndStr
  modifyTransform (\decls -> decls ++ [declaration newName stringAsName])
  return $ (SExpr (ConstSymbol newName String))

transformExpr _ s = return s

transformStmt :: String -> Stmt -> Transform Stmt
transformStmt rndStr stmt =
  case stmt of
    -- TODO go through the expressions because of possible if expression with printf
    Loop stmts -> do
      tStmts <- mapM (transformStmt rndStr) stmts
      return $ Loop tStmts
    While condition stmts -> do
      tStmts <- mapM (transformStmt rndStr) stmts
      return $ While condition tStmts
    VCall sym args -> do
      tArgs <- mapM (transformExpr rndStr) args
      return $ VCall (transformSymbol rndStr sym) tArgs
    anything -> return anything

transformReturn :: Maybe ValueType -> [Stmt] -> [Stmt]
transformReturn Nothing stmts = stmts ++ [Return Nothing]
transformReturn _ [Expr expr] = [Return $ Just expr]
transformReturn _ stmt@[Return _] = stmt
transformReturn ty (stmt:stmts) = stmt : transformReturn ty stmts
transformReturn _ _ = undefined

transformFn :: String -> FnLet -> Transform FnLet
transformFn rndStr (FnLet sym@(FnSymbol _ _ ty) args body) = do
  tBody <- mapM (transformStmt rndStr) body
  tBodyRet <- return $ transformReturn ty tBody
  return $ FnLet (transformSymbol rndStr sym) (map (transformSymbol rndStr) args) tBodyRet
