{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Plotter(ctx, empty, plot) where

import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo

data FileType where
    Png :: FileType

data Context = Context
    { filetype  :: FileType
    , filepath  :: FilePath
    , title_     :: String
    , interval  :: [Double] -- Assumes all functions has a real valued input.
    }

ctx :: FilePath -> String -> [Double] -> Context
ctx = Context Png

title :: Context -> String -> Context
title c title' = c{title_ = title'}

empty :: Context
empty = Context Png "example" "Title" [0,(0.5)..400]

applyExt :: Context -> String
applyExt c =
    case filetype c of
        Png -> filepath c ++ ".png"

plot :: Chart.PlotValue y => Context -> (Double -> y) -> IO ()
plot c fn =
  let fname = applyExt c
  in
  do
    putStrLn $ ("Wrote to file " ++ fname)
    Cairo.toFile Chart.def fname $ do
    Chart.layout_title Chart..= title_ c
    Chart.setColors [Chart.opaque Chart.blue, Chart.opaque Chart.red]
    Chart.plot (Chart.line "am" [[ (x, fn x) | x <- interval c]])
