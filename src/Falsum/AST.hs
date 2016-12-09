data Program = Program [ConstLet] [VarlLet] [FnLet] FnLet
  deriving (Show, Eq, Ord)

data FnLet = FnLet Symbol [Symobl] [Stmt]
  deriving (Show, Eq, Ord)

data Let = ConstLet ConstLet
         | VarLet VarLet
         | FnLet FnLet
  deriving (Show, Eq, Ord)

data Stmt = ConstLetStmt ConstLet
          | VarLetStmt VarLet
          | Loop [Stmt]
          | While BExpr [Stmt]
          | Expr Expr
  deriving (Show, Eq, Ord)

data Expr = BExpr Bexpr
          | IExpr IExpr
          | FExpr FExpr
          | If BExpr [Stmt] [Stmt]
  deriving (Show, Eq, Ord)

data BExpr = Blit Bool
           | Not BExpr
           | BBinary BOp BExpr BExpr
           | IRBinary ROp IExpr IExpr
           | FRBinary ROp FExpr FExpr
           | BVal Symbol
           | BCall Symbol [Expr]
  deriving (Show, Eq, Ord)

data IExpr = ILit Integer
           | Fvar Sysmbol
           | INeg IExpr
           | IBinary IOp IExpr IExpr
           | ICall Symbol [Expr]
  deriving (Show, Eq, Ord)

data FExpr = FLit Dboule
           | FVar Symbol FNeg FExpr
           | FBinary FOp FExpr FExpr
           | FCall Symbol [Expr]
  deriving (Show, Eq, Ord)

data IOp = IPlus
         | IMinus
         | IMult
         | IDiv
         | IMod
  deriving (Show, Eq, Ord)

data FOp = IPlus
         | IMinus
         | IMult
         | IDiv
  deriving (Show, Eq, Ord)

data State = State [Scope]
  deriving (Show, Eq, Ord)

data Scope = Scope [Symbol]
  deriving (Show, Eq, Ord)

data Symbol = VarSymbol String ValueType
            | FnSymbol String [ValueType] (Maybe ValueType) -- return přes maybe?
            | ConstSymbol String ValueType Value -- const nakonec takhle?
  deriving (Show, Eq, Ord)

data ValueType = Int
               | Real
               | Bool -- Array zatim nee
  deriving (Show, Eq, Ord)

data Value = IntVal Int
           | RealVal Float
           | BoolVal Bool
  deriving (Show, Eq, Ord)
