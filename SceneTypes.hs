{-# LANGUAGE TemplateHaskell #-}
module SceneTypes where 

import Data.List
import Data.Monoid
import Control.Lens
import Primitive
import Colour
import Linear
import Object

data CameraType = Perspective | Orthographic
  deriving (Show)

data Light = Light {
  _lPosition :: V4 Double,
  _lColour   :: Colour
}

data SCamera = SCamera {
  cType :: CameraType,
  pos :: V3 Double,
  forward :: V3 Double,
  right :: V3 Double,
  up :: V3 Double
} deriving (Show)

data Scene = Scene {
  _sCamera :: Last SCamera, 
  _sObjects :: [Object],
  _sLights  :: [Light]
}

instance Monoid Scene where
  mempty = Scene mempty mempty mempty
  mappend (Scene c1 o1 l1) (Scene c2 o2 l2) =
    Scene (mappend c1 c2) (mappend o1 o2) (mappend l1 l2)

type SceneObject = [Object]

makeLenses ''Scene
makeLenses ''Light

-------------------------------------------------------------------------------
-- Setter
-------------------------------------------------------------------------------

setObject = set sObjects

setCamera = set sCamera

setLight = set sLights
