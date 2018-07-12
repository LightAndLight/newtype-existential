module Main where

import Criterion.Main

import Exists
import Vec

main :: IO ()
main =
  defaultMain
  [ bench "replicate list" $
    whnf (Prelude.replicate 10000) 'a'
  , bench "replicate vec" $
    whnf (Vec.replicate 10000) 'a'
  ]
