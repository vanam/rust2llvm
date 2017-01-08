module Falsum.Transform where

import           Control.Monad.Trans.State.Lazy
import           Falsum.AST

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

uniqueIdentifer :: Transform String
uniqueIdentifer =
  do
    decls <- get
    return $ ".format." ++ (show . length . declarations $ decls)

modifyTransform :: ([ConstLet] -> [ConstLet]) -> Transform ()
modifyTransform f =
  do
    current <- get
    put $ TransformState (f . declarations $ current)

externalFns :: [FnLet]
externalFns = [DeclareFnLet (VariadicFnSymbol "printf" [String] Nothing)]

transformProgram :: Program -> Program
transformProgram (Program consts vars fns mainFn) =
  Program (consts ++ (declarations . snd $ tFnsAndDecls)) vars ((fst tFnsAndDecls) ++ externalFns)
    (lowLevelMain newMain)
  where
    tFnsAndDecls = runState (mapM transformFn (fns ++ [mainFn])) initialTransformState

newMain :: String
newMain = ".main"

transformSymbol :: Symbol -> Symbol
transformSymbol (FnSymbol "main" [] ty) = FnSymbol newMain [] ty
transformSymbol s = s

transformExpr :: Expr -> Transform Expr
transformExpr (SExpr (ConstSymbol stringAsName String)) = do
  newName <- uniqueIdentifer
  modifyTransform (\decls -> decls ++ [declaration newName stringAsName])
  return $ (SExpr (ConstSymbol newName String))
transformExpr s = return s

transformStmt :: Stmt -> Transform Stmt
transformStmt stmt =
  case stmt of
    Expr expr ->
      case expr of
        IExpr (IIf cond ifBranch elseBranch) -> do
          ifBranchTransformed <- mapM transformStmt ifBranch
          elseBranchTransformed <- mapM transformStmt elseBranch
          return $ Expr (IExpr (IIf cond ifBranchTransformed elseBranchTransformed))
        anything -> return $ Expr anything
    If cond ifBranch elseBranch -> do
      ifBranchTransformed <- mapM transformStmt ifBranch
      case elseBranch of
        Just stmts -> do
          elseTransformed <- mapM transformStmt stmts
          return $ If cond ifBranchTransformed (Just elseTransformed)
        Nothing -> return $ If cond ifBranchTransformed Nothing
    Loop stmts -> do
      tStmts <- mapM transformStmt stmts
      return $ Loop tStmts
    While condition stmts -> do
      tStmts <- mapM transformStmt stmts
      return $ While condition tStmts
    VCall sym args -> do
      tArgs <- mapM transformExpr args
      return $ VCall (transformSymbol sym) tArgs
    anything -> return anything

transformReturn :: Maybe ValueType -> [Stmt] -> [Stmt]
transformReturn Nothing stmts = stmts ++ [Return Nothing]
transformReturn _ [Expr expr] = [Return $ Just expr]
transformReturn _ stmt@[Return _] = stmt
transformReturn ty (stmt:stmts) = stmt : transformReturn ty stmts
transformReturn _ _ = undefined

transformFn :: FnLet -> Transform FnLet
transformFn (FnLet sym@(FnSymbol _ _ ty) args body) = do
  tBody <- mapM transformStmt body
  tBodyRet <- return $ transformReturn ty tBody
  return $ FnLet (transformSymbol sym) (map transformSymbol args) tBodyRet
