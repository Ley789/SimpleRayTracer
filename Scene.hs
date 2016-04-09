{-# LANGUAGE TemplateHaskell #-}
module Scene where 

import qualified Debug.Trace as T
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Lens
import Primitive
import Colour
import Data.Function (on)
import Fast_PPM
import Linear
import Object
import SceneTypes
import Blinn_Phong

-- | Static camera will be used if no carmera was defined.
camera = SCamera Perspective (V3 0.0 0.0 (-20)) (V3 0.0 0 1) (V3 1.0 0 0) (V3 0.0 1 0)

emptyScene :: SceneObject
emptyScene = []

insertPrimitive :: Object -> SceneObject -> SceneObject
insertPrimitive o s = o : s

-- | Convert to Num and divide.
divNum :: (Num a, Fractional a) => Int -> Int -> a
divNum = (/) `on` fromIntegral


--TODO check m n positive
--we are mapping the pixel size to the interval [-1, 1] e.g. left upper corner
--of the picture is (-1, 1), center (0,0) etc.
pixelCoordinates :: (Int, Int) -> [[(Double, Double)]]
pixelCoordinates (m, n) =
  [[(f x m, negate $ f y n) | x <- [0 .. n]] | y <- [0 .. m]]
  where f x a = 2*x `divNum` a - 1

mapTracing :: Ray -> SceneObject -> [Intersection]
mapTracing ray = mapMaybe (rayObjectIntersection ray)

-- | Create image from a scene with given pixel size.
simpleRayTracer :: Scene  -> (Int, Int) -> [[Colour]]
simpleRayTracer s (m, n) = T.trace ("Objects: " ++ show (s ^. sObjects)) $
  filterColour (s ^. sObjects) $ (map.map) (nearestIntersection . (`mapTracing` (s ^. sObjects))) $ generateRays (extractCamera $ s ^. sCamera) (m, n)

-- TODO Change case [] represents no intersection
-- | Extract nearest intersected primitive from list and return color.
nearestIntersection l =
  case l of
    [] -> Nothing
    _  -> Just $ foldl1' nearest l
    where nearest f s
            | rayPointDistance (f ^. ray) (f ^. itPoint) 
              < rayPointDistance (s ^. ray) (s ^. itPoint) = f
            | otherwise = s

filterColour :: SceneObject -> [[Maybe Intersection]] -> [[Colour]]
filterColour s = (map.map) (`intersectionColour` s) 

generateRays c = (map.map) (`generateRay` c) . pixelCoordinates


-- generate ray from camera and pixel coordinates                            
generateRay :: (Double,Double)-> SCamera -> Ray
generateRay x c = 
  case cType c of
    Perspective -> perspecTrans x c
    Orthographic -> orthoTrans x c

perspecTrans x c = Ray (point $ pos c) 
                    (vector $ normalize $ forward c ^+^ 
                     (fst x *^ right c) ^+^  
                     (snd x *^ up c))

orthoTrans x c = Ray (point $ pos c ^+^ forward c ^+^ 
                     (fst x *^ right c) ^+^ 
                     (snd x *^ up c)) (vector $ normalize $ forward c) 


extractCamera x = fromMaybe camera (getLast x)


-------------------------------------------------------------------------------
-- shading functions 
-------------------------------------------------------------------------------

intersectionColour :: Maybe Intersection -> SceneObject -> Colour
intersectionColour i s =
  case i of
    Nothing -> Colour 0 0 0
    Just x  -> if hitLight light (rayToLight light x) s then
                  blinn_phong light x else
                  Colour 0 1 0

rayToLight :: Light -> Intersection -> Ray
rayToLight l i = Ray (origin ^+^ direction) (direction)
  where direction = l ^. lPosition - i ^. itPoint
        origin = i ^. itPoint

hitLight :: Light -> Ray -> SceneObject -> Bool
hitLight l r s = case filter (filterIntersection l r) (mapTracing r s) of
  [] -> True
  _  -> False

filterIntersection :: Light -> Ray -> Intersection -> Bool
filterIntersection l r i = 
  (rayPointDistance r $ i ^. itPoint) > (rayPointDistance r $ l ^.lPosition) 


-------------------------------------------------------------------------------
-- test functions 
-------------------------------------------------------------------------------

light = Light (V4 100 0 (-5) 1) (Colour 1 1 1)
