{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
#-}

module Object ( Marchable, marchPath
              , Object, mindist
              , Position, distance, origin
              , Direction, extend, magnitude
              , PosDir, move, direction
              , Spatial
              ) where

import Direction

class Object o p a where
  mindist  :: o a -> p a -> a

instance (Position p a) => Object p p a where
  mindist a b = distance a b

type Marchable o p d a = (Ord a, Spatial p d a, Object o p a)

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
