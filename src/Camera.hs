{-# LANGUAGE MultiParamTypeClasses, KindSignatures, RecordWildCards #-}
{-|
Module      : Camera
Description : Describing how a viewer might perceive ray-traceable objects

Maintainer  : cress@mit.edu
Stability   : pre-experimental

-}

module Camera where

import Transform

data CameraInfo (p :: * -> *) a =
  CameraInfo { cameraFOV        :: a
             , cameraAspect     :: a
             , cameraNear       :: a
             , cameraMaybeFar   :: Maybe a
             }

class (Transform t p) => Camera t p where
  toClip :: (Floating a) => CameraInfo p a -> t a

