{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , KindSignatures
#-}

{-|
Module      : Shapes
Description : Basic shapes that can be ray-traced.

Maintainer  : cress@mit.edu
Stability   : pre-experimental

These shapes exist independent of any /transformations/.
Therefore, they have no location or position in 3d space, and are centered about the origin.
Shapes can be transformed by turning them into 'TransformedObject' instances and specifying a position.
(See "Transform")
-}

module Shapes where

import Object

{-| Defines an n-dimensional hypersphere, parameterized by:
- a coordinate system @p@
- a measuring system @a@
-}
newtype Sphere (p :: * -> *) a
  -- | The constructor for sphere takes a radius value.
  = Sphere a

instance (Position p) => Object (Sphere p) p where
  mindist (Sphere r) b = distance origin b - r

{-| Defines a hyperplane in the coordinate system p -}
newtype Plane (p :: * -> *) (d :: * -> *) a
  -- | Takes the direction of the \"normal vector\" as an argument
  -- this is in quotes because @d@ is a generalized abstraction of the normal vector.
  = Plane (d a)

instance (Spatial p d) => Object (Plane p d) p where
  mindist (Plane d) = abs . distance origin . project d
