{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
#-}

module Direction ( Position, distance, origin
                 , Direction, extend, magnitude
                 , Spatial, move, direction
                 ) where

import Linear.V3 (V3(..))
import qualified Linear.Metric as L (norm, distance)

class Direction d where
  extend :: (Floating a) => d a -> a -> d a
  magnitude :: (Floating a) => d a -> a

class Position p where
  distance  :: (Floating a) => p a -> p a -> a
  origin    :: (Floating a) => p a

class (Position p, Direction d) => Spatial p d where
  move      :: (Num a) => p a -> d a -> p a
  direction :: (Num a) => p a -> p a -> d a

instance Direction V3 where
  extend d a = (* ((l + a) / l)) <$> d where
    l = L.norm d
  magnitude = L.norm

instance Position V3 where
  distance = L.distance
  origin = V3 0 0 0

instance Spatial V3 V3 where
  move a b = a + b
  direction a b = a - b
