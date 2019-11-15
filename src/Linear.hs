{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module Linear (TransformMatrix(..)) where

import Camera
import Transform
import Spatial

import Linear.V3(V3(..))
import Linear.V4 (V4(..))
import Linear.Projection
import Linear.Matrix (M44, (!*), inv44)
import qualified Linear.Metric as L (norm, distance, project)

-- This needs a type wrapper because M44 is a type synonym.
-- | Matrix which represents a transformation in 3-space on 'V3' instances

newtype TransformMatrix a = Mat (M44 a)
instance Transform TransformMatrix V3 where
  transform (Mat m) (V3 x y z) = (/ w) <$> V3 tx ty tz where
    V4 tx ty tz w = m !* V4 x y z 1
  inverse   (Mat m) = transform (Mat $ inv44 m)

instance Camera TransformMatrix V3 where
  toClip CameraInfo{..} = Mat $ case cameraMaybeFar of
    Nothing  -> infinitePerspective cameraFOV cameraAspect cameraNear
    Just cameraFar -> perspective cameraFOV cameraAspect cameraNear cameraFar

instance Direction V3 where
  extend d a = (* ((l + a) / l)) <$> d where
    l = L.norm d
  magnitude = L.norm
  negated = negate

instance Position V3 where
  distance = L.distance
  origin = V3 0 0 0

instance Spatial V3 V3 where
  move = (+)
  direction = (-)
  project = L.project
