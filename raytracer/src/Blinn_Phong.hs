module Blinn_Phong where

import SceneTypes
import Data.Monoid
import Data.Colour
import Object
import Control.Lens
import Linear
import Data.Colour.RGBSpace
import Primitive
import Control.Applicative

-- TODO we still have no inside autside test, so the cos between the normal
-- and tested vector could be cos (a + 90) if we intersected a primitive from
-- the inside

-- |Constant ambient colour
ambientColour :: RGB Double
ambientColour = RGB 1 1 1

-- | Takes the light colour, diffuse coefficient, normalized direction to light
--   normal vector of the object and returns the diffuse colour reflection.
diffuse :: RGB Double -> Double -> V4 Double -> V4 Double -> RGB Double
diffuse c d l n = pure (*) <*> pure (d * influence n l) <*> c

specular :: RGB Double -> Double -> V4 Double -> V4 Double -> V4 Double -> Double -> RGB Double
specular c d l n v h =  pure (*) <*> pure (d * influence n (bisector v l) **  h) <*> c  

influence :: V4 Double -> V4 Double -> Double
influence n l = max 0 (dot n l)

bisector :: V4 Double -> V4 Double -> V4 Double
bisector v l = normalize $ v + l

blinnPhong :: Light -> Intersection -> RGB Double
blinnPhong li i = pure (+) <*>
   (diffuse lc (prop ^. tDiffuse) l n) <*>
   (specular lc (prop ^. tSpecular) l n v (prop ^. tRoughness))
  where
    lc = li ^. lColour
    lp = li ^. lPosition
    v  = negated . normalize $ i ^. ray . _d
    prop = i ^. itTex . property
    l  = normalize $ lp - (i ^. itPoint)
    n  = i ^. normal
