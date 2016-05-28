module Scene where 

import qualified Debug.Trace as T
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Lens
import Primitive
import Colour
import Data.Function (on)
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


--TODO check m n positive test change
--we are mapping the pixel size to the interval [-0.5, 0.5] e.g. left upper corner
-- | Generates pixel coordinates with m rows and n columns.
pixelCoordinates :: (Int, Int) -> [[(Double, Double)]]
pixelCoordinates (m, n) =
  [[(negate $ f x (m - 1),  f y (n - 1)) | y <- [0 .. n - 1]] | x <- [0 .. m - 1]]
  where f x a = x `divNum` a - 0.5


mapTracing :: Ray -> SceneObject -> [Intersection]
mapTracing r = mapMaybe (rayObjectIntersection r)

-- | Create image from a scene with given pixel size.
simpleRayTracer :: Scene  -> (Int, Int) -> [[Colour]]
simpleRayTracer s (m, n) = 
  case mC of 
    Nothing -> replicate n (replicate m (Colour 0 0 0)) 
    Just c  -> T.trace ("number of rays: " ++ show (sum . map length $ pixelCoordinates (m,n))) $ filterColour sO sL $ (map.map) (nearestIntersection . (`mapTracing` sO)) $ generateRays c (m, n)
    where mC = getLast $ s ^. sCamera
          sO = s ^. sObjects
          sL = s ^. sLights

-- MF: TODO: do the same here as I did in Primitive.hs for frustumIntersection
-- TODO Change case [] represents no intersection
-- | Extract nearest intersected primitive from list and return color.
nearestIntersection :: [Intersection] -> Maybe Intersection
nearestIntersection l =
  case l of
    [] -> Nothing
    _  -> Just $ foldl1' nearest l
    where nearest f s
            | rayPointDistance (f ^. ray) (f ^. itPoint) 
              < rayPointDistance (s ^. ray) (s ^. itPoint) = f
            | otherwise = s

generateRays :: SCamera -> (Int, Int) -> [[Ray]]
generateRays c = (map.map) (`generateRay` c) . pixelCoordinates

-- generate ray from camera and pixel coordinates                            
generateRay :: (Double,Double)-> SCamera -> Ray
generateRay x c = -- T.trace ("\n" ++ show c) $
  case cType c of
    Perspective -> perspecTrans x c
    Orthographic -> orthoTrans x c

perspecTrans :: (Double, Double) -> SCamera -> Ray
perspecTrans x c = Ray (pos c) 
                    (forward c ^+^ 
                    (snd x *^ right c) ^+^  
                    (fst x *^ up c))

orthoTrans :: (Double, Double) -> SCamera -> Ray
orthoTrans x c = Ray (pos c ^+^ forward c ^+^ 
                     (snd x *^ right c) ^+^ 
                     (fst x *^ up c)) (forward c) 

-------------------------------------------------------------------------------
-- shading functions 
-------------------------------------------------------------------------------

filterColour :: SceneObject -> [Light] -> [[Maybe Intersection]] -> [[Colour]]
filterColour s l = (map . map) (maybe (Colour 0 0 0) (intersectionColour s l))

intersectionColour :: SceneObject -> [Light] -> Intersection -> Colour
intersectionColour s ls i =
  i ^. itTex . pigment * (ac + sum (mapMaybe (lightIntersectionColour i s) ls))
  where ac = ambient $ i ^. itTex. property . tAmbient

lightIntersectionColour :: Intersection -> SceneObject -> Light -> Maybe Colour
lightIntersectionColour x s l
  | hitLight l (rayToLight l x) s = Just $ blinnPhong l x
  | otherwise = Nothing

rayToLight :: Light -> Intersection -> Ray
rayToLight l i = Ray (origin ^+^ direction) direction
  where direction = l ^. lPosition ^-^ normalizePoint (i ^. itPoint)
        origin = normalizePoint $ i ^. itPoint

hitLight :: Light -> Ray -> SceneObject -> Bool
hitLight l r s = case filter (not . filterIntersection distL r) (mapTracing r s) of
  [] -> True
  _  -> False
  where distL = distance (r ^. _o) $ l ^. lPosition 

filterIntersection :: Double -> Ray -> Intersection -> Bool
filterIntersection distL r i = 
  rayPointDistance r (i ^. itPoint) > distL
