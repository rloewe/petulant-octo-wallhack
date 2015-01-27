
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
          | If Expr [Expr] [Expr] -- done
          | While Expr [Expr] -- done
          | Plus Expr Expr -- done
          | Minus Expr Expr -- done
          | Times Expr Expr -- done
          | Divide Expr Expr -- done
          | Equal Expr Expr -- done
          | NotEqual Expr Expr -- done
          | Less Expr Expr -- done
          | LessEq Expr Expr -- done
          | Greater Expr Expr -- done
          | GreaterEq Expr Expr -- done
          | StrConcat Expr Expr -- done
          | Var Variable -- done
          | ArrayIndex Expr Expr -- done
          | ArrayLit [Expr] -- done
          | TroolLit Troolean -- done
          | StrLit String -- done (except for escaping)
          | Write Expr -- done
          | GiveBack Expr -- done
          | Constant Type
          deriving (Show)

data Scoping = StaticScoping | DynamicScoping
             deriving (Show)

data Troolean = Yes | No | CouldHappen
              deriving (Show)

data Type = Integer Int
          deriving (Show)
