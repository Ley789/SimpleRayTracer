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

-- \Takes the light colour, diffuse coefficient, normalized direction to light
--  normal vector of the object and returns the diffuse colour reflection.
diffuse :: Colour -> Double -> V4 Double -> V4 Double -> Colour
diffuse c d l n = combine c $ d * influence n l

specular c d l n v h = combine c $ d * (influence n $ bisector v l) **  h  

influence n l = max 0 (dot n l)

bisector v l = normalize $ v + l

combine c d = cmap (* d) c

blinn_phong :: Light -> Intersection -> Colour
blinn_phong li i = 
    (ambient (getProperty ob tAmbient)
    + diffuse lc (getProperty ob tDiffuse) l n
    + specular lc (getProperty ob tSpecular) l n v (getProperty ob tRoughness)) 
      where lc = li ^. lColour
            lp = li ^. lPosition
            ob = i ^. object
            v  = negated . normalize $ getDirection $ i ^. ray 
            l  = normalize $ lp - (i ^. itPoint)
            n  = i ^. normal
