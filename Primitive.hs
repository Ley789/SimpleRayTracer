module Primitive (
                   Primitive(..),
                   Radius,
                   Ray(..),
                   basicCircle,
                   origin,
                   direction,
                   getOrigin,
                   intersection
                   )where

import Linear

--represents an Euclidean coordinate 
type Center = V4 Double
type Origin = V4 Double
type Direction = V4 Double
type Radius = Double 
type Intersection = V4 Double


           
--Types that represent primitives
data Primitive = Sphere     --sphere of raidus 1 with center in the origin
               | Nil 
                deriving(Show)

data Ray = Ray Origin Center
           deriving Show

basicCircle = Sphere

origin :: Double -> Double -> Double -> Origin
origin x y z = V4 x y z 1

direction :: Double -> Double -> Double -> Direction
direction x y z = V4 x y z 0

ray :: Origin -> Direction -> Ray
ray o d = Ray o d


getOrigin (Ray o _) = o


intersection :: Ray -> Primitive -> Maybe Intersection
intersection ray Sphere = sphereIntersection ray


--calculates the possible intersectionpoint 
sphereIntersection (Ray origin direction) 
  | toSquare < 0 = Nothing
  | otherwise = nearestIntersectionSphere origin direction x y
                where a = dot direction direction
                      b = 2 * (dot direction origin)
                      c = (dot origin origin) - 2
                      toSquare = b**2 - 4 * a * c
                      x = (-b) + sqrt toSquare
                      y = (-b) - sqrt toSquare  

--There can be 2 intersections with a sphere and 
--this function returns the nearest one
nearestIntersectionSphere origin direction res1 res2
  | res1 >= 0 && res2 >= 0 = Just $ intersection $ min res1 res2
  | res1 >= 0              = Just $ intersection res1
  | res2 >= 0              = Just $ intersection res2
  | otherwise              = Nothing
  where intersection t = origin ^+^(t *^ direction)



