module Scene where 

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Lens
import Primitive
import Data.Colour.RGBSpace
import Data.Function (on)
import Data.Ord   (comparing)
import Codec.Picture.Types
import Control.Applicative
import Linear
import Object
import SceneTypes
import Blinn_Phong
import GHC.Float (double2Float)

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
simpleRayTracer :: Scene -> (Int, Int) -> DynamicImage
simpleRayTracer s (m, n) =
  case mC of 
    Nothing -> imageCreator n m $ replicate n (replicate m (pure 0)) 
    Just c  -> 
       imageCreator n m $ shading sO sL $ (map.map) (nearestIntersection . (`mapTracing` sO)) $ generateRays c (m, n)
    where mC = getLast $ s ^. sCamera
          sO = s ^. sObjects
          sL = s ^. sLights

-- | Extract nearest intersected primitive from list and return color.
nearestIntersection :: [Intersection] -> Maybe Intersection
nearestIntersection l =
  case l of
    [] -> Nothing
    _  -> Just $ minimumBy 
           (comparing (\x -> rayPointDistance (x ^. ray) (x ^. itPoint))) l

generateRays :: SCamera -> (Int, Int) -> [[Ray]]
generateRays c = (map.map) (`generateRay` c) . pixelCoordinates

-- generate ray from camera and pixel coordinates                            
generateRay :: (Double,Double)-> SCamera -> Ray
generateRay x c =
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

shading :: SceneObject -> [Light] -> [[Maybe Intersection]] -> [[RGB Double]]
shading s l = (map . map) (maybe (pure 0) (intersectionColour s l))

intersectionColour :: SceneObject -> [Light] -> Intersection -> RGB Double
intersectionColour s ls i =
  pure (*) <*> pm <*> (pure (+) <*> (pure (ac*) <*> ambientColour) <*> c)
  where ac = i ^. itTex. property . tAmbient
        pm = i ^. itTex . pigment
        tmp = mapMaybe (lightIntersectionColour i s) ls
        c = if null tmp then pure 0 else foldl1' com tmp
        com c1 c2 = pure (+) <*> c1 <*> c2

lightIntersectionColour :: Intersection -> SceneObject -> Light -> Maybe (RGB Double)
lightIntersectionColour x s l
  | hitLight l (rayToLight l x) s = Just $ blinnPhong l x
  | otherwise = Nothing

rayToLight :: Light -> Intersection -> Ray
rayToLight l i = Ray (origin  ^+^ e *^ direction)  direction
  where direction = normalize $ (l ^. lPosition) ^-^ origin
        origin = (i ^. itPoint)
        e      = 0.00001

hitLight :: Light -> Ray -> SceneObject -> Bool
hitLight l r s = case filter (not . filterIntersection distL r) (mapTracing r s) of
  [] -> True
  _  -> False
  where distL = distance (r ^. _o . _xyz) (l ^. lPosition . _xyz)

filterIntersection :: Double -> Ray -> Intersection -> Bool
filterIntersection distL r i = 
  rayPointDistance r (i ^. itPoint) > distL


-- | Convention the colour array must have r-rows and c-columns.
imageCreator :: Int -> Int -> [[RGB Double]] -> DynamicImage
imageCreator r c colourList = ImageRGBF $ generateImage (pixelRenderer colourList) r c

pixelRenderer :: [[RGB Double]] -> Int -> Int -> PixelRGBF
pixelRenderer l x y = PixelRGBF (double2Float r) (double2Float g) (double2Float  b)
  where RGB r g b = (l !! y) !! x
