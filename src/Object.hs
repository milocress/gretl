{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
           , KindSignatures
#-}

module Object ( Marchable, marchPath
              , Object, mindist
              , Position, distance, origin
              , Direction, extend, magnitude
              , Spatial, move, direction
              ) where

import Direction

class Object o p where
  mindist  :: (Floating a) => o a -> p a -> a

instance (Position p) => Object p p where
  mindist a b = distance a b

type Marchable o p d a = (Floating a, Ord a, Spatial p d, Object o p)

{-# INLINE marchPath #-}
marchPath :: (Marchable o p d a) => p a -> d a -> o a -> a -> a -> (Maybe (p a))
marchPath point dir obj mn mx = marchPath' dir where
  marchPath' d = if md < mn
    then Just $ point'
    else if md > mx
         then Nothing
         else marchPath' d' where
    md = mindist obj point'
    point' = point `move` d
    d' = extend d md
