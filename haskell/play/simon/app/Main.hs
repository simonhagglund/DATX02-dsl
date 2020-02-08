{-# LANGUAGE GADTs #-}
module Main where

import Lib

main :: IO ()
main = someFunc

type TF s = s -> s

