{-# LANGUAGE GADTs, ExistentialQuantification, DeriveDataTypeable #-}
module Main where
import Data.Typeable

data Value =
    VInt Int | VBool Bool deriving (Eq, Show)

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

evalSimple :: Typeable a => Simple a -> Env -> Either String a
evalSimple expr env = case expr of
    IVal n -> Right n
    IBool b -> Right b
    IVar x -> case lookup x env of
        Nothing -> Left ("Unbound variable" ++ x)
        Just (ValueWrapper v) -> case cast v of
            Nothing -> Left "Type mismatch"
            Just e -> evalSimple e env
    IAdd n m -> do
        v1 <- evalSimple n env
        v2 <- evalSimple m env
        return (v1 + v2)
    IMul n m -> do
        v1 <- evalSimple n env
        v2 <- evalSimple m env
        return (v1 * v2)
    IsZero n -> do
        v <- evalSimple n env
        return (v == 0)
    IDiv n m -> do
        b <- evalSimple (IsZero m) env
        if not b then do
            v1 <- evalSimple n env
            v2 <- evalSimple m env
            return (v1 `div` v2)
        else
            Left "Error: division by zero"
    If b e1 e2 -> do
        guard <- evalSimple b env
        if guard then evalSimple e1 env else evalSimple e2 env
    Let x e1 e2 -> let env2 = (x, ValueWrapper e1):env in evalSimple e2 env2


main :: IO ()
main = do
    _ <- print (evalSimple (IDiv (IVal 5) (IVal 0)) [])
    -- _ <- print (evalSimple (IsZero (IsZero (IVal 3))))
    _ <- print (evalSimple (If (IBool False) (IVal 3) (IVal 4)) [])
    _ <- print (evalSimple (Let "x" (IVal 3) (IAdd (IVar "x") (IVal 5))) [])
    return ()
