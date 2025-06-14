{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.Typeable

data Value a =
    VInt Int | VBool Bool | VFun String (Simple a) Env

instance Eq (Value a) where
    (==) :: Value a -> Value a -> Bool
    VInt x == VInt y = x == y
    VBool x == VBool y = x == y
    _ == _ = False --ignore functions

instance Show (Value a) where
    show :: Value a -> String
    show (VInt x) = show x
    show (VBool x) = show x
    show VFun {}  = "<function>"

data ValueWrapper where
    ValueWrapper :: Typeable a => Simple a -> ValueWrapper

type Env = [(String, ValueWrapper)]

data Simple a where
    IVal :: Int -> Simple Int
    IBool :: Bool -> Simple Bool
    IVar :: String -> Simple a
    IAdd :: Simple Int -> Simple Int -> Simple Int
    IMul :: Simple Int -> Simple Int -> Simple Int
    IDiv :: Simple Int -> Simple Int -> Simple Int
    IsZero :: Simple Int -> Simple Bool
    If :: Simple Bool -> Simple a -> Simple a -> Simple a
    Let :: String -> Simple a -> Simple a -> Simple a
    Fun :: String -> Simple a -> Simple a
    Apply :: Simple a -> Simple a -> Simple a

evalSimple :: Typeable a => Simple a -> Env -> Either String (Value a)
evalSimple expr env = case expr of
    IVal n -> Right (VInt n)
    IBool b -> Right (VBool b)
    IVar x -> case lookup x env of
        Nothing -> Left ("Unbound variable" ++ x)
        Just (ValueWrapper v) -> case cast v of
            Nothing -> Left "Type mismatch"
            Just e -> evalSimple e env
    IAdd n m -> do
        v1 <- evalSimple n env
        v2 <- evalSimple m env
        case (v1, v2) of
            (VInt arg1, VInt arg2) -> return (VInt (arg1 + arg2))
            _ -> Left "Type mismatch"
    IMul n m -> do
        v1 <- evalSimple n env
        v2 <- evalSimple m env
        case (v1, v2) of
            (VInt arg1, VInt arg2) -> return (VInt (arg1 * arg2))
            _ -> Left "Type mismatch"
    IsZero n -> do
        v <- evalSimple n env
        return (VBool (v == VInt 0))
    IDiv n m -> do
        b <- evalSimple (IsZero m) env
        case b of
            VBool True -> Left "Error: division by zero"
            VBool False -> do
                v1 <- evalSimple n env
                v2 <- evalSimple m env
                case (v1, v2) of
                    (VInt arg1, VInt arg2) -> return (VInt (arg1 `div` arg2))
                    _ -> Left "Type mismatch: arguments of Div must be integers"
            _ -> Left "Type mismatch"
    If b e1 e2 -> do
        guard <- evalSimple b env
        case guard of
            VBool True -> evalSimple e1 env
            VBool False -> evalSimple e2 env
            _ -> Left "Type mismatch: guard has to be boolean"
    Let x e1 e2 -> let env2 = (x, ValueWrapper e1):env in evalSimple e2 env2
    Fun ide body -> Right (VFun ide body env)
    Apply f arg -> do
        fclosure <- evalSimple f env
        case fclosure of
            VFun par body fenv -> evalSimple body ((par,ValueWrapper arg):fenv)
            _ -> Left "No function in apply"


main :: IO ()
main = do
    _ <- print (evalSimple (IDiv (IVal 5) (IVal 0)) [])
    _ <- print (evalSimple (If (IBool False) (IVal 3) (IVal 4)) [])
    _ <- print (evalSimple (Let "x" (IVal 3) (IAdd (IVar "x") (IVal 5))) [])
    _ <- print (evalSimple (Let "f" (Fun "x" (IAdd(IVar "x") (IVal 5))) (Apply (IVar "f") (IVal 8))) [])
    return ()
