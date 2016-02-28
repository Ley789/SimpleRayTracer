module Scene where 

import qualified Debug.Trace as T
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Lens
import Primitives
import Material
import Colour
import Data.Function (on)
import Fast_PPM
import Linear

data Object = Object Primitive RayModifier 

data RayModifier = RM {
  inversOTrans :: M44 Double -- ^ Inverse transformation matrix of the object
} deriving Show


data SceneItem = SIObject Object | SICamera SCamera

type Scene = [SceneItem]

-- to rename used because type Scene changes to add light and camera and be more flexible
type TmpTyName = [Object]


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

emptyScene :: TmpTyName
emptyScene = []

insertPrimitive :: Object -> TmpTyName -> TmpTyName
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


--we transform the ray instead of the primitive.
--So we can use the standard intersection equations
-- Was macht das, und warum macht es das?
transformRay :: Ray -> RayModifier -> Ray
transformRay (Ray o d) (RM trans) =
  Ray newOrigin newDirection
  where newOrigin    = (trans !* point o) ^. _xyz
        newDirection = (trans !* vector d) ^. _xyz


-- | Intersect ray with object and return color and
-- distance from ray origin to object intersection.
rayObjectIntersection :: Ray -> Object -> Maybe (Material, Double)
rayObjectIntersection ray (Object p rm) = do
  res <- intersection (transformRay ray rm) p
  return (colour 1 0 0, getOrigin ray `distance` res)

mapTracing :: Ray -> TmpTyName -> [(Material,Double)]
mapTracing ray = mapMaybe (rayObjectIntersection ray)

-- | Create image from a scene with given pixel size.
simpleRayTracer :: Scene  -> (Int, Int) -> [[Colour]]
simpleRayTracer s (m, n) = filterColour $ (map.map) (nearestIntersection . (`mapTracing` extractObject s)) $ generateRays (extractCamera s) (m, n)

-- | Extract nearest intersected primitive from list and return color.
nearestIntersection l =
  case l of
    [] -> noIntersection
    _  -> fst $ foldl1' nearest l
    where nearest f@(m,x) s@(m',y) 
            | x < y = f
            | otherwise = s

filterColour = (map.map) getColour
generateRays c = (map.map) (`generateRay` c) . pixelCoordinates

-- generate ray from camera and pixel coordinates                            
generateRay :: (Double,Double)-> SCamera -> Ray
generateRay x c = 
  case cType c of
    Perspective -> perspecTrans x c
    Orthographic -> orthoTrans x c

perspecTrans x c = Ray (pos c) 
                    (normalize $ forward c ^+^ 
                     (fst x *^ right c) ^+^  
                     (snd x *^ up c))

orthoTrans x c = Ray (pos c ^+^ forward c ^+^ 
                     (fst x *^ right c) ^+^ 
                     (snd x *^ up c)) (normalize $ forward c) 

-- ask for better solutions
-- maybe using filter
-- or extract in triple
extractObject [] = []
extractObject (x:xs) = 
  case x of
    SIObject o -> o: extractObject xs
    _          -> extractObject xs 
extractCamera [] = camera
extractCamera (x:xs) =
  case x of
    SICamera c -> c
    _          -> extractCamera xs
