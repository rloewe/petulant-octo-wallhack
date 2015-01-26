
module PowAST where

type Program = [Fun]

data Fun = Fun { funName    :: Id
               , funScoping :: Scoping
               , funParams  :: [Param]
               , funBody    :: [Statement] }

type Id = String
type FunName = Id
type Variable = Id
type Param = Variable
type Statement = Expr
type Args = [Expr]

data Expr = Assign Variable Expr
          | Call Args FunName
          | If Expr [Expr] [Expr]
          | While Expr [Expr]
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Less Expr Expr
          | LessEq Expr Expr
          | Greater Expr Expr
          | GreaterEq Expr Expr
          | StrConcat Expr Expr
          | Var Variable
          | ArrayIndex Expr Expr
          | ArrayLit [Expr]
          | BoolLit Troolean
          | StrLit String
          | Write Expr
          | GiveBack Expr
          deriving (Show)

data Scoping = StaticScoping | DynamicScoping
             deriving (Show)

data Troolean = Yes | No | CouldHappen
              deriving (Show)