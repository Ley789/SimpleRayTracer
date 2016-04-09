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
simpleRayTracer s (m, n) = 
  case mC of 
    Nothing -> replicate n (replicate m (Colour 0 0 0)) 
    Just c  -> T.trace ("Objects: " ++ show (s ^. sObjects)) $ filterColour sO sL $ (map.map) (nearestIntersection . (`mapTracing` sO)) $ generateRays c (m, n)
    where mC = getLast $ s ^. sCamera
          sO = s ^. sObjects
          sL = s ^. sLights

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

generateRays c = (map.map) (`generateRay` c) . pixelCoordinates


-- generate ray from camera and pixel coordinates                            
generateRay :: (Double,Double)-> SCamera -> Ray
generateRay x c = -- T.trace ("\n" ++ show c) $
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

-------------------------------------------------------------------------------
-- shading functions 
-------------------------------------------------------------------------------
filterColour :: SceneObject -> [Light] -> [[Maybe Intersection]] -> [[Colour]]
filterColour s l i = (map.map) (intersectionColour s l) i 

intersectionColour :: SceneObject -> [Light] -> Maybe Intersection -> Colour
intersectionColour s ls i = 
  case i of
    Nothing -> Colour 0 0 0
    Just x  -> getColour (x ^. object) * 
               foldl' (+) (Colour 0 0 0) (map (lightIntersectionColour x s) ls)

lightIntersectionColour :: Intersection -> SceneObject -> Light -> Colour
lightIntersectionColour x s l = -- T.trace ("\n" ++ show l) $ 
   if hitLight l (rayToLight l x) s then
      blinn_phong l x else
      Colour 0 0 0

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
