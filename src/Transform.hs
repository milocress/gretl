{-# LANGUAGE MultiParamTypeClasses
           , KindSignatures
           , FlexibleInstances
#-}
{-|
Module      : Transfrom
Description : Representing abstract transformations in general coordinate spaces

Maintainer  : cress@mit.edu
Stability   : pre-experimental

-}
module Transform ( Transform, transform, inverse
                 , TransformedObject (..)
                 ) where

import Object

-- | A type @t@ that is capable of representing an invertible transformation
-- in a coordinate system @p@
class Transform t p where
  -- | Takes a transform representation of type @t@ in measurement space @a@
  -- and returns a function which transforms a point in coordinate space @p@.
  transform :: (Fractional a) => t a -> p a -> p a
  -- | Takes a transform representation and returns a function which inverts
  -- the original transform, satisfying the identity:
  --
  -- > transform t . inverse t == id
  inverse   :: (Fractional a) => t a -> p a -> p a

-- | Any 'Object' which has a transform applied to it can be proved to be an
-- object. @TransformedObject@ represents the delayed application of a transform
-- on an object.
data TransformedObject t o a = TransformedObject (t a) (o a)

instance (Transform t p, Object o p) => Object (TransformedObject t o) p where
  mindist (TransformedObject t o) p = mindist o $ inverse t p

