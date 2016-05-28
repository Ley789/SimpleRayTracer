module Blinn_Phong where

import SceneTypes
import Colour
import Object
import Control.Lens
import Linear
import Primitive

-- |Constant ambient colour
ambientColour :: Colour
ambientColour = cpromote 1

ambient :: Double -> Colour
ambient = combine ambientColour

-- | Takes the light colour, diffuse coefficient, normalized direction to light
--   normal vector of the object and returns the diffuse colour reflection.
diffuse :: Colour -> Double -> V3 Double -> V3 Double -> Colour
diffuse c d l n = combine c $ d * influence n l

specular :: Colour -> Double -> V3 Double -> V3 Double -> V3 Double -> Double -> Colour
specular c d l n v h = combine c $ d * influence n (bisector v l) **  h  

influence :: V3 Double -> V3 Double -> Double
influence n l = max 0 (dot n l)

bisector :: V3 Double -> V3 Double -> V3 Double
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
    l  = normalize $ lp - normalizePoint (i ^. itPoint)
    n  = normalizePoint $ i ^. normal
