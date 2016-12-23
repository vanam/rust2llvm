module Falsum.AST where

data Program = Program [ConstLet] [VarLet] [FnLet] FnLet
  deriving (Show, Eq, Ord)

data TopLevel = TopFnLet FnLet
              | TopConstLet ConstLet
              | TopVarLet VarLet
  deriving (Show, Eq, Ord)

data FnLet = FnLet Symbol [Symbol] [Stmt]
           | DeclareFnLet Symbol [Symbol] Bool
  deriving (Show, Eq, Ord)

data ConstLet = ConstLet Symbol Value
  deriving (Show, Eq, Ord)

data VarLet = VarLet Symbol Expr
  deriving (Show, Eq, Ord)

data Let = ConstLetLet ConstLet
         | VarLetLet VarLet
         | FnLetLet FnLet
  deriving (Show, Eq, Ord)

data Stmt = ConstLetStmt ConstLet
          | VarLetStmt VarLet
          | Loop [Stmt]
          | While BExpr [Stmt]
          | Return (Maybe Expr)
          | Expr Expr
          | VCall Symbol [Expr]
  deriving (Show, Eq, Ord)

data Expr = BExpr BExpr
          | IExpr IExpr
          | FExpr FExpr
          | SExpr Symbol
          | If BExpr [Stmt] (Maybe [Stmt])
  deriving (Show, Eq, Ord)

data BExpr = BLit Bool
           | BNot BExpr
           | BBinary BOp BExpr BExpr
           | IRBinary ROp IExpr IExpr
           | FRBinary ROp FExpr FExpr
           | BVar Symbol
           | BCall Symbol [Expr]
           | BAssign LValue BExpr
  deriving (Show, Eq, Ord)

data IExpr = ILit Integer
           | IVar Symbol
           | INeg IExpr
           | IBinary IOp IExpr IExpr
           | ICall Symbol [Expr]
           | IAssign LValue IExpr
  deriving (Show, Eq, Ord)

data FExpr = FLit Float
           | FVar Symbol
           | FNeg FExpr
           | FBinary FOp FExpr FExpr
           | FCall Symbol [Expr]
           | FAssign LValue FExpr
  deriving (Show, Eq, Ord)

data BOp = BAnd
         | BOr
  deriving (Show, Eq, Ord)

data IOp = IPlus
         | IMinus
         | IMult
         | IDiv
         | IMod
         | IAnd
         | IOr
         | IXor
  deriving (Show, Eq, Ord)

data FOp = FPlus
         | FMinus
         | FMult
         | FDiv
  deriving (Show, Eq, Ord)

data ROp = Less
         | LessEqual
         | Greater
         | GreaterEqual
         | Equal
         | NotEqual
  deriving (Show, Eq, Ord)

data ParseState = ParseState [Scope] (Maybe ValueType)
  deriving (Show, Eq, Ord)

data Scope = Scope [Symbol]
  deriving (Show, Eq, Ord)

data Symbol = GlobalVarSymbol String ValueType
            | VarSymbol String ValueType
            | FnSymbol String (Maybe ValueType)
            | ConstSymbol String ValueType
  deriving (Show, Eq, Ord)

data ValueType = Int
               | Real
               | Bool
               | String
               | ArrayBool
               | ArrayInt
               | ArrayReal
  deriving (Show, Eq, Ord)

data Value = IntVal Int
           | RealVal Float
           | BoolVal Bool
           | StringVal String
  deriving (Show, Eq, Ord)

data LValue = LValue Symbol
            | BAccess Symbol IExpr
            | IAccess Symbol IExpr
            | FAccess Symbol IExpr
  deriving (Show, Eq, Ord)
