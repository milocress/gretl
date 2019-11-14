{-# LANGUAGE MultiParamTypeClasses, KindSignatures, RecordWildCards #-}
{-|
Module      : Camera
Description : Describing how a viewer might perceive ray-traceable objects

Maintainer  : cress@mit.edu
Stability   : pre-experimental

-}

module Camera where

import Transform
import Linear.V3(V3(..))
import Linear.Projection

data CameraInfo (p :: * -> *) a =
  CameraInfo { cameraFOV        :: a
             , cameraAspect     :: a
             , cameraNear       :: a
             , cameraMaybeFar   :: Maybe a
             }

class (Transform t p) => Camera t p where
  toClip :: (Floating a) => CameraInfo p a -> t a

instance Camera TransformMatrix V3 where
  toClip CameraInfo{..} = Mat $ case cameraMaybeFar of
    Nothing  -> infinitePerspective cameraFOV cameraAspect cameraNear
    Just cameraFar -> perspective cameraFOV cameraAspect cameraNear cameraFar

