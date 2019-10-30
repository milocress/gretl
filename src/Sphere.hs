{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , KindSignatures
#-}
module Sphere where

import Object

data Sphere (p :: * -> *) a = Sphere a

instance (Position p) => Object (Sphere p) p where
  mindist (Sphere r) b = distance origin b - r
