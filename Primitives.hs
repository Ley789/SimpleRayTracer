module Primitives (
                   Primitive,
                   Radius,
                   Ray(..),
                   basicCircle,
                   origin,
                   direction,
                   getOrigin,
                   intersection,
                   normalize,
                   translate,
                   distance
                   )where

import Linear

--represents an euklidean coordinate 
type Center = V3 Double
type Origin = V3 Double
type Direction = V3 Double
type Radius = Double 
type Intersection = V3 Double


           

--todo change to radius 1 and apply matrix multiplication
data Primitive = Circle Center Radius

data Ray = Ray Origin Center

instance Show Primitive where
      show (Circle x y) = "Center: " ++ show x ++" radius: "++ show y


basicCircle = Circle (V3 0.0 0.0 0.0) 1

translate ::Primitive -> V3 Double -> Primitive
translate (Circle p r) t = Circle (translatePoint p t) r

origin :: Double -> Double -> Double -> Origin
origin x y z = V3 x y z

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
                            where v =  origin ^-^ center 
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
                                      intersection = origin ^+^(coefficient *^ direction)

--only works if at least 1 element is positive
minPos x y 
         | x >= 0 && y >=0 = min x y
         | x >= 0 =  x
         | otherwise = y
               
    
skalarProduct v w = sumPoint $ liftI2 (*) v w

componentFunction f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')

translatePoint ::(Num a) => V3 a -> V3 a -> V3 a
translatePoint x y = x ^+^ y

sumPoint (V3 x y z) = x + y + z



