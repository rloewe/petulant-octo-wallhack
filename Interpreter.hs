{-# LANGUAGE Rank2Types #-}

module Interpreter
( interpret
) where

import PowAST
import qualified Data.Map as M
import Control.Monad
import System.Random

type SymTab = M.Map Id Value
type FunTab = M.Map Id Fun

interpret :: Program -> IO ()
interpret p = eval vtab ftab p
  where ftab = foldr (\x acc -> M.insert (funName x) x acc) M.empty p
        vtab = M.empty

eval :: SymTab -> FunTab -> Program -> IO ()
eval vtab ftab program =
  case M.lookup "main" ftab of
    Nothing -> error "Main not found!"
    Just main -> evalFun vtab ftab main

evalBlock :: SymTab -> FunTab -> [Expr] -> IO (Value, SymTab)
-- TODO: Initial value...?
evalBlock vtab ftab exprs = foldM foo (ValueTroolean CouldHappen, vtab) exprs
    where foo (value, vtab') expr = evalExpr vtab' ftab expr

evalFun :: SymTab -> FunTab -> Fun -> IO ()
evalFun vtab ftab fun = case funBody fun of
  [] -> return ()
  (statement : rest) -> do
    (_, vtab') <- evalExpr vtab ftab statement
    evalFun vtab' ftab (fun { funBody = rest })

evalArith :: Value -> Value -> (Int -> Int -> Int) -> Value
evalArith (ValueInteger e1) (ValueInteger e2) op = ValueInteger $ op e1 e2
evalArith _ _ _ = error "Can't handle those types!"

evalCompare :: SymTab -> FunTab -> Expr -> Expr -> (forall a. Ord a => a -> a -> Bool) -> IO (Value, SymTab)
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
setIndex n value [] = (ValueTroolean CouldHappen) : setIndex (n-1) value []
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

  If cond thenBranch elseBranch -> do
    (condValue, vtab') <- evalExpr vtab ftab cond
    case condValue of
      ValueTroolean No -> evalBlock vtab ftab elseBranch
      ValueTroolean CouldHappen -> do
        gen <- newStdGen
        let (bool, _) = random (gen) :: (Bool, StdGen)
        if bool then evalBlock vtab ftab thenBranch
                else evalBlock vtab ftab elseBranch
      _ -> evalBlock vtab ftab thenBranch

  While cond exprs -> do
    (cond', vtab') <- evalExpr vtab ftab cond
    case cond' of
      ValueTroolean No -> return (ValueTroolean CouldHappen, vtab')
      ValueTroolean CouldHappen -> do
        gen <- newStdGen
        let (bool, _) = random (gen) :: (Bool, StdGen)
        if bool then (do
            (result, vtab'') <- evalBlock vtab ftab exprs
            evalExpr vtab'' ftab $ While cond exprs)
                else return (ValueTroolean CouldHappen, vtab')
      _ -> do
        (result, vtab'') <- evalBlock vtab ftab exprs
        evalExpr vtab'' ftab $ While cond exprs

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
    (value, vtab') <- evalExpr vtab ftab (Equal e1 e2)
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
      (ValueString s1, ValueString s2) -> ValueString $ s1 ++ s2
      (_, _) -> error "Why you no gibe strings"
    return (newstring, vtab'')

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
  ArrayLit exprs -> do
    foldM (\(valueArr, vtab') expr -> do
      (value, vtab'') <- evalExpr vtab' ftab expr
      let updatedArray = insertValueInArr value valueArr
      return (updatedArray, vtab'')) (ValueArray [], vtab) exprs

  Write e -> do
    (value, vtab') <- evalExpr vtab ftab e
    writeValue value
    return (ValueTroolean CouldHappen, vtab')

  -- NOTE: Calls right now just giveback the last statement
  --       This should probably be changed once we have proper giveback
  Call argExprs fname -> do
    case M.lookup fname ftab of
      Just fun -> do
        (argValues, vtab') <- foldM evalExprs ([], vtab) argExprs
        baseVtab <- return $ case funScoping fun of
          StaticScoping -> M.empty
          DynamicScoping -> vtab'
        let actualArgs = bindArgs (funParams fun) argValues baseVtab
        (result, vtab'') <- evalBlock actualArgs ftab (funBody fun)
        return $ case funScoping fun of
          StaticScoping -> (result, vtab')
          DynamicScoping -> (result, vtab'')
      Nothing -> error $ fname ++ " is wat"
    where
      evalExprs (values, vtab') expr = do
        (value, vtab'') <- evalExpr vtab' ftab expr
        return (values++[value], vtab'')

      bindArgs :: [Param] -> [Value] -> SymTab -> SymTab
      bindArgs params values vtab =
        if length params == length values
        then foldl (\vtab' (param, value) ->
          M.insert param value vtab') vtab (zip params values)
        else error "number of arguments are wat"

  -- TODO: Actually giveback
  GiveBack expr -> evalExpr vtab ftab expr

  Constant value -> return (value, vtab)
  TroolLit trool -> return (ValueTroolean trool, vtab)
  StrLit str -> return (ValueString str, vtab)
  IntLit int -> return (ValueInteger int, vtab)

  Var id -> case M.lookup id vtab of
    Just value -> return (value, vtab)
    Nothing -> error $ "~" ++ id ++ " is not defined, at least it was wat"

insertValueInArr :: Value -> Value -> Value
insertValueInArr value (ValueArray a) = ValueArray (a ++ [value])
insertValueInArr _ _ = error "Why you no gibe array"

writeValue :: Value -> IO ()
writeValue x = writeValue' x >>= \ _ -> putStrLn ""
    where
        writeValue' (ValueString v) = putStr v
        writeValue' (ValueInteger v) = putStr $ show v
        writeValue' (ValueTroolean Yes) = putStr "true"
        writeValue' (ValueTroolean No) = putStr "false"
        writeValue' (ValueTroolean CouldHappen) = putStr "maybe"
        writeValue' (ValueArray values) = do
          putStr "#"
          writeValueArray values
          return ()
            where
              writeValueArray :: [Value] -> IO ()
              writeValueArray [] = return ()
              writeValueArray [v] = writeValue' v
              writeValueArray (v:vs) = writeValue' v >>= (\_ ->
                putStr ", "                       >>= (\_ ->
                writeValueArray vs
                ))
