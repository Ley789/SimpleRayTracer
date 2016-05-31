{-# LANGUAGE TemplateHaskell #-}
module SceneTypes where 

import Data.Monoid
import Control.Lens
import Data.Colour.RGBSpace
import Linear
import Object

data CameraType = Perspective | Orthographic
  deriving (Show)

data Light = Light {
  _lPosition :: V4 Double,
  _lColour   :: RGB Double
} deriving(Show)


data SCamera = SCamera {
  cType :: CameraType,
  pos :: V4 Double,
  forward :: V4 Double,
  right :: V4 Double,
  up :: V4 Double
} deriving (Show)

data Scene = Scene {
  _sCamera :: Last SCamera, 
  _sObjects :: [Object],
  _sLights  :: [Light]
} deriving(Show)

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
setObject :: [Object] -> Scene -> Scene
setObject = set sObjects

setCamera :: Last SCamera -> Scene -> Scene
setCamera = set sCamera

setLight :: [Light] -> Scene -> Scene
setLight = set sLights
