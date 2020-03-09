{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Laplace where

data F_TF = F_Y | F_U | F_G

data T_TF = T_Y | T_U | T_G

-- Syntax
data Expr domain where
    Id      :: Expr domain
    Signal  :: domain       -> Expr domain
    Const   :: Integer      -> Expr domain
    (:*:)   :: Expr domain  -> Expr domain
    (:+:)   :: Expr domain  -> Expr domain

-- Addition binds the terms in the list.
newtype Poly domain = Poly [Expr domain]

-- Sematics is to replace T_TF members with members of F_TF.
--   The structure will be perserved, but the terms will change,
eval :: Poly T_TF -> Poly F_TF
eval poly = undefined

instance Show (Poly T_TF) where
    show ps = undefined -- Here we show derivatives.

instance Show (Poly F_TF) where
    show ps = undefined -- Here we show s to the power of index.

laplace = eval
-- And done with parial derivative laplace

-- Concider for a moment the convoultion of g `cov` u
-- In frequency domain this is the same as G `times` U


