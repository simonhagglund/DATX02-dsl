{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import           Control.Invertible.BiArrow (invert)
import           Control.Invertible.Functor as BF
import qualified Data.Invertible.Prelude    as DIP
import           Lib
import           Prelude                    as P

main :: IO ()
main = putStrLn "Hej"


data Expr s where
  X :: Expr s
  E :: Expr s
  Const :: s -> Expr s
  Pow :: Int -> Expr s
  (:+:) :: Expr s -> Expr s -> Expr s
  (:-:) :: Expr s -> Expr s -> Expr s
  (:*:) :: Expr s -> Expr s -> Expr s
  (:/:) :: Expr s -> Expr s -> Expr s
  (:^:) :: Expr s -> Expr s -> Expr s
  (:~:) :: Expr s -> Expr s -> Expr s -- Convolution
  Dirac :: Expr s -> Expr s
  HStep :: Expr s -> Expr s
  Log   :: Expr s -> Expr s
  Recip :: Expr s -> Expr s
  deriving (Show)

newtype TDom = T Double
  deriving (Num)

newtype SDom = S Double
  deriving (Num)


instance Show TDom where
  show (T t) = show t ++ "t"
instance Show SDom where
  show (S s) = show s ++ "s"

instance P.Functor Expr where
  fmap f (a :+: b) = fmap f a :+: fmap f b where fmap = P.fmap
  fmap f (a :*: b) = fmap f a :*: fmap f b where fmap = P.fmap
  fmap f (a :-: b) = fmap f a :-: fmap f b where fmap = P.fmap
  fmap f (a :/: b) = fmap f a :/: fmap f b where fmap = P.fmap
  fmap f (a :^: b) = fmap f a :^: fmap f b where fmap = P.fmap
  --fmap f (Const x) = Const (fmap f x)
  fmap f (Dirac x) = Dirac (fmap f x) where fmap = P.fmap
  fmap f (HStep x) = HStep (fmap f x) where fmap = P.fmap
  --fmap f a         = a


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
  fmap = P.fmap


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




data TransfFunc s where
  TransfFunc :: String -> Expr s -> TransfFunc s

tf1, tf2 :: TransfFunc SDom
tf1 = TransfFunc "F" (Recip (Pow 1 :+: Pow 2))
tf2 = TransfFunc "G" (Recip (Pow 2))

ex1 :: Expr TDom
ex1 = Const 1
ex2 = neg (E :^: neg X)


evalExpr :: (Num s, Fractional s, Floating s) => Expr s -> s -> s
evalExpr e x = case e of
  (Const i) -> i
  a :+: b   -> evalExpr a x + evalExpr b x
  a :*: b   -> evalExpr a x * evalExpr b x
  a :-: b   -> evalExpr a x - evalExpr b x
  a :/: b   -> evalExpr a x / evalExpr b x
  a :^: b   -> evalExpr a x ** evalExpr b x
  E         -> fromRational 2.71828
  X         -> x
  Pow n     -> x ** fromIntegral n
  Recip a   -> recip (evalExpr a x)
  Log a     -> log (evalExpr a x)

instance Num s => Num (Expr s) where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  fromInteger = Const . fromInteger
  negate = neg
  abs    = undefined
  signum = undefined

instance Fractional s => Fractional (Expr s) where
  (/) = (:/:)
  recip = Recip
  fromRational = Const . fromRational

instance Floating s => Floating (Expr s) where
  pi = Const pi
  exp = (E :^:)

instance BF.Functor Expr where
  fmap bf (a :+: b) = fmap bf a :+: fmap bf b where fmap = BF.fmap
  fmap bf (a :~: b) = fmap bf a :*: fmap bf b where fmap = BF.fmap
  fmap bf X         = Recip $ Pow 2

ex3 = X :~: X

-- Conclusion: Laplace cannot be easily implemented in this way as a bijective functor
{-
The bijection is applied to the mapped function itself, and not the functor
mapping. The following is possible
invert DIP.id BF.<$> ex3
i.e. inverting a function (in this case the bijective id function) and mapping
it on the functor expression ex3. The :~:->:+: mapping cannot be reversed this
way. There may be other class structures possible to try.
-}
