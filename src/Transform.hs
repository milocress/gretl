{-# LANGUAGE MultiParamTypeClasses
#-}
module Transform ( Transform, transform, inverse, compose
                 ) where

class Transform p where
  transform :: p a -> p a
  inverse   :: p a -> p a
  compose   :: (p a -> p a) -> (p a -> p a) -> p a -> p a
  compose = (.)
