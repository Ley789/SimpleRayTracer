import Data.List
import Primitives
import Material
import Colour
import Data.Function (on)
import Fast_PPM

type Object = (Material,Primitive)
type Scene = [Object]
type Position = Point3D
type Forward = Point3D
type Right = Point3D
type Up = Point3D

--static camera to present the result
data Camera = Camera Position Forward Right Up
camera = Camera (Point3D 0 0 (-10)) (Point3D 0 0 1) (Point3D 1 0 0) (Point3D 0 1 0)
position (Camera p _ _ _) = p
forward (Camera _ f _ _ ) = f
right (Camera _ _ r _) = r
up (Camera _ _ _ u) = u
infinity = read "Infinity" 


emptyScene :: Scene
emptyScene = []

insertPrimitive :: Object -> Scene -> Scene
insertPrimitive (m,p) s = (m,p) : s

--this function does a division after the convertion from Int to Double
division :: Int -> Int -> Double
division = (/) `on` fromIntegral

--TODO check for positive m n
--we are mapping the pixel size to the intervall [-1, 1] e.g. left uper corner
--of the picture is (-1, 1), center (0,0) ect.
getPixelCoordinates :: Int -> Int -> [(Double, Double)]
getPixelCoordinates m n = let range = take (max m n) [0,1..] in
                          [(2*(division x (m-1)) -1 ,-(2* (division y (n-1)) -1))
                          |y <- range, x <-range, x < m, y< n]


--genereate ray from camera and pixelcoordinates                            
generateRay :: (Double,Double) -> Ray
generateRay x = Ray (position camera) 
                    (normalize $ forward camera `addPoints` 
                     (multPointComp (fst x) (right camera)) `addPoints` 
                     (multPointComp (snd x) (up camera)))


generateRays ::Int -> Int -> [Ray]
generateRays m n = map generateRay $ getPixelCoordinates m n

--trace returns the distance the material and the distance to 
--the intersected primitive
trace :: Ray -> Object -> (Material, Double)
trace ray (m,p) = let res = intersection ray p in
                  case res of
                  Nothing -> (noIntersection, infinity)
                  Just a -> (m, getOrigin ray `distanceBetween` a)


rayTracing :: Ray -> Scene -> [(Material,Double)]
rayTracing ray scene = map (trace ray) scene
 

--combinding the functions to return pixels
simpleRayTracer :: Scene -> Int -> Int -> [[Colour]]
simpleRayTracer s m n = filterColour $ splitList m $ map (foldl1' nearest) $ map (`rayTracing` s) $ generateRays m n

nearest f@(m,x) s@(m',y) 
                      | x < y = f
                      | otherwise = s


filterColour = map (map (\x -> getColour $ fst x)) 
splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n l = take n l : splitList n (drop n l)
