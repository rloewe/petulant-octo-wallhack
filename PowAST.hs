
module PowAST where

type Program = [Fun]

data Fun = Fun { funName    :: Id
               , funScoping :: Scoping
               , funParams  :: [Param]
               , funBody    :: [Statement] }
               deriving (Show)

type Id = String
type FunName = Id
type Variable = Id
type Param = Variable
type Statement = Expr
type Args = [Expr]

data Expr = Assign Expr Expr -- done
          | Call Args FunName -- done
          | If Expr [Expr] [Expr]
          | While Expr [Expr]
          | Plus Expr Expr -- done (maybe)
          | Minus Expr Expr -- done (maybe)
          | Times Expr Expr -- done (maybe)
          | Divide Expr Expr -- done (maybe)
          | Less Expr Expr -- done (sorta)
          | LessEq Expr Expr -- done (sorta)
          | Greater Expr Expr -- done
          | GreaterEq Expr Expr -- done
          | StrConcat Expr Expr
          | Var Variable -- done
          | ArrayIndex Expr Expr
          | ArrayLit [Expr]
          | BoolLit Troolean
          | StrLit String
          | Write Expr
          | GiveBack Expr
          | Constant Type
          deriving (Show)

data Scoping = StaticScoping | DynamicScoping
             deriving (Show)

data Troolean = Yes | No | CouldHappen
              deriving (Show)

data Type = Integer Int
          deriving (Show)
