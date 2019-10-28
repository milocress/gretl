{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
#-}
module Sphere where

import Object

data Sphere p a = Sphere a

instance (Num a, Position p a) => Object (Sphere (p a)) p a where
  mindist (Sphere r) b = distance origin b - r
