{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
#-}

module Direction ( Position, distance, origin
                 , Direction, extend, magnitude
                 , PosDir, move, direction
                 , Spatial
                 ) where

import Linear.V3 (V3(..))
import qualified Linear.Metric as L (norm, distance)

class Direction d a where
  extend :: d a -> a -> d a
  magnitude :: d a -> a

class Position p a where
  distance  :: p a -> p a -> a
  origin    :: p a

class PosDir p d a where
  move      :: p a -> d a -> p a
  direction :: p a -> p a -> d a

instance (Floating a) => Direction V3 a where
  extend d a = (* ((l + a) / l)) <$> d where
    l = L.norm d
  magnitude = L.norm

instance (Floating a) => Position V3 a where
  distance = L.distance
  origin = V3 0 0 0

instance (Floating a) => PosDir V3 V3 a where
  move a b = a + b
  direction a b = a - b

type Spatial p d a = (Direction d a, Position p a, PosDir p d a)
