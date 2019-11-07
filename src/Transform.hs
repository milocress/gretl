{-# LANGUAGE MultiParamTypeClasses
           , KindSignatures
           , FlexibleInstances
#-}
module Transform ( Transform, transform, inverse
                 , TransformedObject (..)
                 ) where

import Object

import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Matrix (M44, (!*), inv44)

class Transform t p where
  transform :: (Fractional a) => t a -> p a -> p a
  inverse   :: (Fractional a) => t a -> p a -> p a

data TransformedObject t o a = TransformedObject (t a) (o a)

instance (Transform t p, Object o p) => Object (TransformedObject t o) p where
  mindist (TransformedObject t o) p = mindist o $ inverse t p

-- This needs a type wrapper because M44 is a type synonym.
newtype TransformMatrix a = Mat (M44 a)
instance Transform TransformMatrix V3 where
  transform (Mat m) (V3 x y z) = (/ w) <$> V3 tx ty tz where
    V4 tx ty tz w = m !* (V4 x y z 1)
  inverse   (Mat m) p = transform (Mat $ inv44 m) p
