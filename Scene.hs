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


type Scene = (Last SCamera, [Object])

-- to rename used because type Scene changes to add light and camera and be more flexible
type SceneObject = [Object]


data SCamera = SCamera {
  cType :: CameraType,
  pos :: V3 Double,
  forward :: V3 Double,
  right :: V3 Double,
  up :: V3 Double
}

data CameraType = Perspective | Orthographic

-- | Static camera will be used if no carmera was defined.
camera = SCamera Perspective (V3 0.0 0.0 (-10)) (V3 0.0 0 1) (V3 1.0 0 0) (V3 0.0 1 0)

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

mapTracing :: Ray -> SceneObject -> [(Ray, Object, V4 Double, V4 Double)]
mapTracing ray = mapMaybe (rayObjectIntersection ray)

-- | Create image from a scene with given pixel size.
simpleRayTracer :: Scene  -> (Int, Int) -> [[Colour]]
simpleRayTracer s (m, n) = filterColour $ (map.map) (nearestIntersection . (`mapTracing` extractObject s)) $ generateRays (extractCamera s) (m, n)

-- TODO Change case [] represents no intersection
-- | Extract nearest intersected primitive from list and return color.
nearestIntersection l =
  case l of
    [] -> noIntersection
    _  -> getObject $ foldl1' nearest l
    where nearest f@(r1,_,_,p1) s@(r2,_,_,p2) 
            | rayPointDistance r1 p1 < rayPointDistance r2 p2 = f
            | otherwise = s

filterColour = (map.map) getColour
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

extractObject (_,x) = x 

extractCamera (x,_) = fromMaybe camera (getLast x)

noIntersection = Object Nil mempty
