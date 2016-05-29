module Blinn_Phong where

import SceneTypes
import Colour
import Object
import Control.Lens
import Linear
import Primitive

-- TODO we still have no inside autside test, so the cos between the normal
-- and tested vector could be cos (a + 90) if we intersected a primitive from
-- the inside

-- |Constant ambient colour
ambientColour :: Colour
ambientColour = cpromote 1

ambient :: Double -> Colour
ambient = combine ambientColour

-- | Takes the light colour, diffuse coefficient, normalized direction to light
--   normal vector of the object and returns the diffuse colour reflection.
diffuse :: Colour -> Double -> V4 Double -> V4 Double -> Colour
diffuse c d l n = combine c $ d * influence n l

specular :: Colour -> Double -> V4 Double -> V4 Double -> V4 Double -> Double -> Colour
specular c d l n v h = combine c $ d * influence n (bisector v l) **  h  

influence :: V4 Double -> V4 Double -> Double
influence n l = max 0 (dot n l)

bisector :: V4 Double -> V4 Double -> V4 Double
bisector v l = normalize $ v + l

combine :: Colour -> Double -> Colour
combine c d = cmap (* d) c

blinnPhong :: Light -> Intersection -> Colour
blinnPhong li i = 
    diffuse lc (prop ^. tDiffuse) l n
  + specular lc (prop ^. tSpecular) l n v (prop ^. tRoughness)
  where
    lc = li ^. lColour
    lp = li ^. lPosition
    v  = negated . normalize $ i ^. ray . _d
    prop = i ^. itTex . property
    l  = normalize $ lp - (i ^. itPoint)
    n  = i ^. normal
