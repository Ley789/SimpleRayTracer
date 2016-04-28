module Blinn_Phong where

import qualified Debug.Trace as T
import SceneTypes
import Colour
import Object
import Control.Lens
import Linear
import Primitive

-- |Constant ambient colour
ambientColour = cpromote 1

ambient d = combine ambientColour d

-- | Takes the light colour, diffuse coefficient, normalized direction to light
--   normal vector of the object and returns the diffuse colour reflection.
diffuse c d l n = combine c $ d * influence n l

specular c d l n v h = combine c $ d * (influence n $ bisector v l) **  h  

influence :: V3 Double -> V3 Double -> Double
influence n l = max 0 (dot n l)

bisector v l = normalize $ v + l

combine c d = cmap (* d) c

blinn_phong :: Light -> Intersection -> Colour
blinn_phong li i = 
    ambient (prop ^. tAmbient)
  + diffuse lc (prop ^. tDiffuse) l n
  + specular lc (prop ^. tSpecular) l n v (prop ^. tRoughness)
  where
    lc = li ^. lColour
    lp = li ^. lPosition
    v  = negated . normalize $ getDirection $ i ^. ray
    prop = i ^. itTex . property
    l  = normalize $ lp - normalizePoint (i ^. itPoint)
    n  = normalizePoint $ i ^. normal
