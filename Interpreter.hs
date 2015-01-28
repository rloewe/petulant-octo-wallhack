module Interpreter where

import PowAST
import qualified Data.Map as M
import Control.Monad

interpret :: Program -> IO ()
interpret p = eval vtab ftab p
  where ftab = foldr (\x acc -> M.insert (funName x) x acc) M.empty p
        vtab = M.empty

-- [
--   Fun {
--     funName = "add",
--     funScoping = StaticScoping,
--     funParams = ["a","b"],
--     funBody = [GiveBack (Plus (Var "a") (Var "b"))]
--   }
-- ]

type SymTab = M.Map Id Value
type FunTab = M.Map Id Fun

eval :: SymTab -> FunTab -> Program -> IO ()
eval vtab ftab program =
  case M.lookup "main" ftab of
    Nothing -> error "Main not found!"
    Just main -> evalFun vtab ftab main

evalFun :: SymTab -> FunTab -> Fun -> IO ()
evalFun vtab ftab fun = case funBody fun of
  [] -> return ()
  (statement : rest) -> do
    vtab' <- evalExpr vtab ftab statement
    evalFun vtab' ftab (fun { funBody = rest })

evalArith :: Value -> Value -> (Int -> Int -> Int) -> Value
evalArith (ValueInteger e1) (ValueInteger e2) op = ValueInteger $ op e1 e2
evalArith _ _ _ = error "Can't handle those types!"

evalCompare :: Ord a => SymTab -> FunTab -> Expr -> Expr -> (a -> a -> Bool) -> IO (Value, SymTab)
evalCompare vtab ftab e1 e2 cmp = do
  (e1', vtab') <- evalExpr vtab ftab e1
  (e2', vtab'') <- evalExpr vtab' ftab e2
  trool <- return $ case (e1', e2') of
    (ValueInteger x, ValueInteger y) -> if cmp x y then ValueTroolean Yes else ValueTroolean No
    (ValueString x, ValueString y) -> if cmp x y then ValueTroolean Yes else ValueTroolean No
    (_, _) -> ValueTroolean CouldHappen
  return (trool, vtab'')

setIndex :: Int -> Value -> [Value] -> [Value]
setIndex 0 value values = value : values
setIndex n value [] = (Troolean CouldHappen) : setIndex (n-1) value values
setIndex n value (v : values) = v : setIndex (n-1) value values

evalExpr :: SymTab -> FunTab -> Statement -> IO (Value, SymTab)
evalExpr vtab ftab expr = case expr of
  Assign e1 e2 -> do
    case e1 of
      (Var v) -> do
        (e2', vtab') <- evalExpr vtab ftab e2
        let vtab'' = M.insert v e2' vtab'
        return (e2', vtab'')
      (ArrayIndex index (Var v)) -> do
        (index', vtab') <- evalExpr vtab ftab index
        (e2', vtab'') <- evalExpr vtab' ftab e2
        let array = M.lookup v vtab''
        case array of
          Just (ValueArray array') -> do
            vtab''' <- case index' of
              (ValueInteger i) -> return $ M.insert v (ValueArray (setIndex i e2' array')) vtab''
              (ValueTroolean CouldHappen) -> do
                i <- getStdRandom(randomR (0, (length array') - 1))
                return $ M.insert v (ValueArray (setIndex i e2' array')) vtab''
              _ -> error "index is wat"
            return (e2', vtab''')
          _ -> error "unbound/not array waat"
      _ -> error "You no can make ωign here"
  Call Args FunName -> undefined
  If Expr [Expr] [Expr] -> undefined
  While Expr [Expr] -> undefined
  Plus e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    let result = evalArith e1' e2' (+)
    return (result, vtab'')
  Minus e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    let result = evalArith e1' e2' (-)
    return (result, vtab'')
  Times e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    let result = evalArith e1' e2' (*)
    return (result, vtab'')
  Divide e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    let result = evalArith e1' e2' div
    return (result, vtab'')
  Equal e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    case (e1', e2') of
      (ValueInteger i1, ValueInteger i2) ->
        if i1 == i2
        then return (ValueTroolean Yes, vtab'')
        else return (ValueTroolean No, vtab'')
      (ValueTroolean i1, ValueTroolean i2) ->
        if i1 == i2
        then return (ValueTroolean Yes, vtab'')
        else return (ValueTroolean No, vtab'')
      (ValueString i1, ValueString i2) ->
        if i1 == i2
        then return (ValueTroolean Yes, vtab'')
        else return (ValueTroolean No, vtab'')
      (ValueArray i1, ValueArray i2) ->
        if i1 == i2
        then return (ValueTroolean Yes, vtab'')
        else return (ValueTroolean No, vtab'')
      (i1, i2) -> return (ValueTroolean CouldHappen, vtab'')
  NotEqual e1 e2 -> do
    (value, vtab') <- evalExpr (Equal e1 e2)
    value' <- case value of
      ValueTroolean Yes -> return $ ValueTroolean No
      ValueTroolean No -> return $ ValueTroolean Yes
      _ -> return value
    return (value', vtab')
  Less e1 e2 -> evalCompare vtab ftab e1 e2 (<)
  LessEq e1 e2 -> evalCompare vtab ftab e1 e2 (<=)
  Greater e1 e2 -> evalCompare vtab ftab e1 e2 (>)
  GreaterEq e1 e2 -> evalCompare vtab ftab e1 e2 (>=)
  StrConcat e1 e2 -> do
    (e1', vtab') <- evalExpr vtab ftab e1
    (e2', vtab'') <- evalExpr vtab' ftab e2
    newstring <- return $ case (e1', e2') of
      (ValueString s1, ValueString s2) -> s1 ++ s2
      (_, _) -> error "Why you no gibe strings"
  Var v -> case M.lookup v vtab of
    Just (id, value) -> return (value, vtab)
    Nothing -> error "Wat is variable"
  ArrayIndex index arr -> do
    (index', vtab') <- evalExpr vtab ftab index
    (arr', vtab'') <- evalExpr vtab' ftab arr
    value <- case (index', arr') of
      (ValueInteger x, ValueArray arr'') -> return $ arr'' !! x
      (ValueTroolean CouldHappen, ValueArray arr'') -> do
        randomIndex <- getStdRandom(randomR (0, (length arr'') - 1))
        return $ arr'' !! randomIndex
      (_, _) -> error "Index is wat"
    return (value, vtab'')
  Write e -> do
    (value, vtab') <- evalExpr vtab ftab e
    writeExpr value
    return (ValueTroolean CouldHappen, vtab')
  GiveBack Expr -> undefined
  Constant value -> value
  ArrayLit exprs -> do
    foldM (\(valueArr, vtab') expr -> do
      (value, vtab'') <- evalExpr vtab' ftab expr
      return (insertValueInArr value valueArr, vtab'')) (ValueArray []) exprs
  TroolLit trool -> return (ValueTroolean trool, vtab)
  StrLit str -> return (ValueString str, vtab)
  IntLit int -> return (ValueInteger int, vtab)

insertValueInArr :: Value -> Value -> Value
insertValueInArr value (ValueArray a) = ValueArray (a ++ [value])
insertValueInArr _ _ = error "Why you no gibe array"

writeExpr :: Value -> IO ()
writeExpr (ValueString v) = putStr v
writeExpr (ValueInteger v) = putStr $ show v
writeExpr (ValueTroolean Yes) = putStr "true"
writeExpr (ValueTroolean No) = putStr "false"
writeExpr (ValueTroolean CouldHappen) = putStr "maybe"
writeExpr (ValueArray values) = do
  putStr "#"
  writeArray values
  return ()
    where writeExprArray [] = ""
          writeExprArray [v] = writeExpr v
          writeExprArray (v:vs) = writeExpr v >>= (\_ ->
            putStr ", "                       >>= (\_ ->
            writeExprArray vs
            ))
