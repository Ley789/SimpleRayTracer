module Primitives (
                   Primitive,
                   Point3D(..),
                   Radius,
                   Ray(..),
                   basicCircle,
                   origin,
                   direction,
                   getOrigin,
                   intersection,
                   distanceBetween,
                   normalize,
                   addPoints,
                   multPointComp,
                   translate,
                   distance
                   )where

--represents an euklidean coordinate
data Point3D = Point3D Double Double Double 
type Center = Point3D
type Origin = Point3D
type Direction = Point3D
type Radius = Double
type Intersection = Point3D
data Ray = Ray Origin Direction
           deriving(Show)
data Primitive = Circle Center Radius

instance Show Point3D where
      show (Point3D x y z) = "x=" ++ show x ++ " y=" ++  show y ++ " z=" ++ show z
instance Show Primitive where
      show (Circle x y) = "Center: " ++ show x ++" radius: "++ show y


basicCircle :: Radius -> Primitive
basicCircle r = Circle (Point3D 0 0 0) r

translate :: Primitive -> Point3D -> Primitive
translate (Circle p r) t = Circle (translatePoint p t) r

origin :: Double -> Double -> Double -> Origin
origin x y z = Point3D x y z

direction :: Double -> Double -> Double -> Direction
direction = origin

ray :: Origin -> Direction -> Ray
ray o d = Ray o d


getOrigin (Ray o _) = o

intersection :: Ray -> Primitive -> Maybe Intersection
intersection ray c@(Circle center radius)
                       = sphereIntersection ray c

--calculates the possible intersectionpoint 
sphereIntersection (Ray origin direction) (Circle center radius) 
                       = if toSquare < 0 then 
                            Nothing      else  
                            nearestIntersectionSphere origin direction x y
                            where v = componentFunction (-) origin center 
                                  p = skalarProduct v direction 
                                  toSquare = p**2 - (skalarProduct v v - radius**2)
                                  x = (-p) + sqrt toSquare
                                  y = (-p) - sqrt toSquare  

--there are always 2 intersections with a sphere and 
--this function returns the nearest one
nearestIntersectionSphere origin direction res1 res2 =
                                if res1 < 0 && res2 < 0 then 
                                Nothing           else
                                Just intersection
                                where coefficient = minPos res1 res2
                                      intersection = componentFunction (+) origin (multPointComp coefficient direction)

--only works if at least 1 element is positive
minPos x y 
         | x >= 0 && y >=0 = min x y
         | x >= 0 =  x
         | otherwise = y
               
    
skalarProduct v w = sumPoint $ componentFunction (*) v w


componentFunction f (Point3D x y z) (Point3D x' y' z') = Point3D (f x x') (f y y') (f z z')

translatePoint  = addPoints

addPoints = componentFunction (+)

sumPoint (Point3D x y z) = x + y + z

distance (Point3D x y z) = sqrt (x*x + y*y + z*z)
x `distanceBetween` y = distance $ componentFunction (-) y x

normalize p@(Point3D x y z) = Point3D (x/dist) (y/dist) (z/dist)
                            where dist = distance p

functionOnComponents f (Point3D x y z) = Point3D (f x) (f y) (f z)

multPointComp x = functionOnComponents (*x) 
