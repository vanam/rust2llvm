module Falsum.Transform where

import           Control.Monad.Trans.State.Lazy
import           Falsum.AST

import           Debug.Trace

println :: Show a => a -> Transform ()
println msg = trace (show msg) $ return ()

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
declaration name val = ConstLet (ConstSymbol name String) (StringVal $ val ++ "\NUL")

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
externalFns = [DeclareFnLet (VariadicFnSymbol "printf" [String] (Just Int))]

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
          condTrans <- transformStmt (Expr (BExpr cond))
          ifBranchTrans <- mapM transformStmt ifBranch
          elseBranchTrans <- mapM transformStmt elseBranch
          let Expr (BExpr c) = condTrans
          return $ Expr (IExpr (IIf c ifBranchTrans elseBranchTrans))
        BExpr (IRBinary op expr1 expr2) -> do
          e1Trans <- transformStmt (Expr (IExpr expr1))
          e2Trans <- transformStmt (Expr (IExpr expr2))
          let Expr (IExpr e1) = e1Trans
          let Expr (IExpr e2) = e2Trans
          return $ Expr $ BExpr (IRBinary op e1 e2)
        anything -> return $ Expr anything
    If cond ifBranch elseBranch -> do
      ifBranchTransformed <- mapM transformStmt ifBranch
      condTrans <- transformStmt (Expr (BExpr cond))
      case condTrans of
        Expr (BExpr c)    -- strip transformed condition
         ->
          case elseBranch of
            Just stmts -> do
              elseTransformed <- mapM transformStmt stmts
              return $ If c ifBranchTransformed (Just elseTransformed)
            Nothing -> return $ If c ifBranchTransformed Nothing

    Loop stmts -> do
      tStmts <- mapM transformStmt stmts
      return $ Loop tStmts
    While condition stmts -> do
      condTrans <- transformStmt (Expr (BExpr condition))
      tStmts <- mapM transformStmt stmts
      let Expr (BExpr c) = condTrans
      return $ While c tStmts
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
