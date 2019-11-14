{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
           , KindSignatures
           , ExistentialQuantification
#-}

{-|
Module      : Object
Description : System for tracing paths in general coordinate systems with general notions of distance.

Maintainer  : cress@mit.edu
Stability   : pre-experimental


-}

module Object ( Marchable, marchPath
              , Object, mindist
              , Position, distance, origin
              , Direction, extend, magnitude
              , Spatial, move, direction, project
              ) where

import Spatial

-- | An 'Object' must give us some way of orienting ourselves to it.
class Object o p where
  -- | Given a point in coordinate space @p@, with measuring system @a@,
  -- an 'Object' must define a minimum distance estimator between itself
  -- and that point.
  mindist :: (Floating a, Ord a) => o a -> p a -> a

-- Any point can be trivially \"promoted\" into an object.
instance (Position p) => Object p p where
  mindist = distance

-- | An object -- note this is not the same as an 'Object' -- can be
-- ray-traced if it satisfies the following criteria:
--
-- - it must have a continuous measurement system ('Floating', 'Ord')
--
-- - it must have a directional coordinate system ('Spatial')
--
-- - it must supply a minimum distance estimator  ('Object')
type Marchable o p d a = (Floating a, Ord a, Spatial p d, Object o p)

-- | A collection of objects with identical coordinate systems
-- and measurement systems can be grouped heterogenously into an
-- @OList@, which is itself an object. This gives us a way to \"fuse\"
-- 'Object' instances together.
data OList p a = forall o. Object o p => OList [o a]
instance Object (OList p) p where
  mindist (OList os) p = minimum $ (`mindist` p) <$> os

-- | Uses the raymarching algorithm to find the point of collision
-- between an object and a point in a given direction.
marchPath :: (Marchable o p d a)
  => p a -- ^ Initial point
  -> d a -- ^ Direction of \"motion\"
  -> o a -- ^ Target object
  -> a   -- ^ Minimum distance to march
  -> a   -- ^ Maximum distance to march
  -> Maybe (p a) -- ^ @Just@ Location of collision or @Nothing@ if no
  -- collision occurs.
marchPath point dir obj mn mx = marchPath' dir where
  marchPath' d
    | md < mn = Just point'
    | md > mx = Nothing
    | otherwise = marchPath' d'
    where md = mindist obj point'
          point' = point `move` d
          d' = extend d md
{-# INLINE marchPath #-}
