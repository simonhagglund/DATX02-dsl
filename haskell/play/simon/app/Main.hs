{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lib

main :: IO ()
main = someFunc

type TF s = s -> s

data TFExpr s where
    Add         :: TFExpr s -> TFExpr s -> TFExpr s
    Sub         :: TFExpr s -> TFExpr s -> TFExpr s
    Mul         :: TFExpr s -> TFExpr s -> TFExpr s
    Neg         :: TFExpr s -> TFExpr s
    Abs         :: TFExpr s -> TFExpr s
    Signum      :: TFExpr s -> TFExpr s
    FromInteger :: Integer  -> TFExpr s
    Tf          :: s -> TFExpr s
    --Cos         :: TFExpr s -> TFExpr s
    --Sin         :: TFExpr s -> TFExpr s
    --Derive
    --Integral

tf :: s -> TFExpr s
tf = Tf

type TFExp s = TFExpr (TF s)

instance Num s => Num (TFExp s) where
    (+)         = Add
    (-)         = Sub
    (*)         = Mul
    negate      = Neg
    abs         = Abs
    signum      = Signum
    fromInteger = FromInteger


evalTF :: Num s => TFExp s -> TF s
evalTF (Tf f)   = f
evalTF (Add f g)  = \s -> (evalTF f) s + (evalTF g) s
evalTF (Mul f g)  = \s -> (evalTF f) s * (evalTF g) s
evalTF (Sub f g)  = \s -> (evalTF f) s - (evalTF g) s
evalTF (Neg f)    = negate . evalTF f
evalTF (Abs f)    = abs . evalTF f
evalTF (Signum f) = signum . evalTF f
--evalTF (Cos f)    = cos . evalTF f
--evalTF (Sin f)    = sin . evalTF f

-- | 2*s + (s +2)*3
example = tf (2*) + tf (+2) * tf (const 3)

test = evalTF example $ 2 -- == 16
