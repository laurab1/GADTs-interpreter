{-# LANGUAGE GADTs #-}
module Main where

data Value =
    VInt Int | VBool Bool deriving (Eq, Show)

type Env = [(String, Value)]

data Simple a where
    IVal :: Int -> Simple Int
    IBool :: Bool -> Simple Bool
    IAdd :: Simple Int -> Simple Int -> Simple Int
    IMul :: Simple Int -> Simple Int -> Simple Int
    IDiv :: Simple Int -> Simple Int -> Simple Int
    IsZero :: Simple Int -> Simple Bool
    If :: Simple Bool -> Simple a -> Simple a -> Simple a
    Let :: String -> Simple a -> Simple a -> Simple a

evalSimple :: Simple a -> Env -> Either String a
evalSimple expr env = case expr of
    IVal n -> Right n
    IBool b -> Right b
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
        if guard then do
            v1 <- evalSimple e1 env
            return v1
        else do
            v2 <- evalSimple e2 env
            return v2
    Let x e1 e2 -> let v1 = evalSimple e1 env
                       env2 = (x,v1):env
                   in evalSimple e2 env2

main :: IO ()
main = do
    _ <- print (evalSimple (IDiv (IVal 5) (IVal 0)))
    -- _ <- print (evalSimple (IsZero (IsZero (IVal 3))))
    _ <- print (evalSimple (If (IBool False) (IVal 3) (IVal 4)))
    return ()
