{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import           Lib

main :: IO ()
main = putStrLn "Hej"


data Expr s where
  X :: Expr s
  E :: Expr s
  Const :: s -> Expr s
  (:+:) :: Expr s -> Expr s -> Expr s
  (:-:) :: Expr s -> Expr s -> Expr s
  (:*:) :: Expr s -> Expr s -> Expr s
  (:/:) :: Expr s -> Expr s -> Expr s
  (:^:) :: Expr s -> Expr s -> Expr s
  Dirac :: Expr s -> Expr s
  HStep :: Expr s -> Expr s
  Log   :: Expr s -> Expr s
  deriving (Show)

newtype TDom = T Double
  deriving (Num)

newtype SDom = S Double
  deriving (Num)


-- rule
-- Expr s
-- [a] ..
-- fmap show [1..3]
-- fmap show (Const 1 :+: Const 2 :+: Const 3) = (Const "1" :+: )
-- fmap show [1..3]
-- foldl (\b -> a) b [a]


instance Functor Expr where
  fmap f (a :+: b) = fmap f a :+: fmap f b
  fmap f (a :*: b) = fmap f a :*: fmap f b
  fmap f (a :-: b) = fmap f a :-: fmap f b
  fmap f (a :/: b) = fmap f a :/: fmap f b
  fmap f (a :^: b) = fmap f a :^: fmap f b
  fmap f (Const x) = Const (fmap f x)
  fmap f (Dirac x) = Dirac (fmap f x)
  fmap f (HStep x) = HStep (fmap f x)
  fmap f a         = a


neg :: Num s => Expr s -> Expr s
neg = (Const 0 :-:)

laplace :: Expr TDom -> Expr SDom
laplace = laplace' . fmap transform where
  transform :: TDom -> SDom
  transform (T x) = S x
  laplace' :: Num s => Expr s -> Expr s
  laplace' (Dirac t)         = Const 1
  laplace' (HStep t)         = X :^: Const (-1)
  laplace' X                 = X :^: Const (-2)
  laplace' (E :^: (a :*: X)) = (X :-: a) :^: Const (-1)
  laplace' (a :+: b)         = laplace' a :+: laplace' b
  laplace' (Const x :*: a)   = Const x :*: laplace' a


der :: Num n => Expr n -> Expr n
der (Const _) = Const 0
der X         = Const 1
der (a :+: b) = der a :+: der b
der (a :*: b) = (der a :*: b) :+: (a :*: der b)
der (a :^: b) = a :^: b :*: ((b :*: der a :/: a) :+: (Log a :*: der b))

{-instance (Show s, Num s) => Show (Expr s) where
  show e = case e of
    X       -> "x"
    E       -> "e"
    Const n -> show n
    o a b = show a ++ op ++ show b where
      op = case o of
      (:+:) -> "+"
      (:-:) -> "-"
      (:*:) -> "*"
      (:/:) -> "/"
      (:^:) -> "^"-}
