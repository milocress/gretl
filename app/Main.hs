{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}

module Main where

import Direction
import Linear.V3

main :: IO ()
main = print $ extend (V3 0.5 1 1) (1 :: Double)
