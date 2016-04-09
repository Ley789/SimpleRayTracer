module Primitive (
                   Primitive(..),
                   Radius,
                   Ray(..),
                   basicCircle,
                   origin,
                   direction,
                   getOrigin,
                   getDirection,
                   getNormal,
                   intersection
                   )where

import qualified Debug.Trace as T 
import Linear
import Control.Lens
--represents an Euclidean coordinate 
type Center = V4 Double
type Origin = V4 Double
type Direction = V4 Double
type Radius = Double 
type Intersection = V4 Double


           
--Types that represent primitives
data Primitive = Sphere     --sphere of raidus 1 with center in the origin
               | Box        --box with length and width 1. Aligned at axes.
               | Cone Radius Radius
                deriving(Show)

data Ray = Ray Origin Direction
           deriving Show

basicCircle = Sphere

origin :: Double -> Double -> Double -> Origin
origin x y z = V4 x y z 1

direction :: Double -> Double -> Double -> Direction
direction x y z = V4 x y z 0

ray :: Origin -> Direction -> Ray
ray o d = Ray o d


getOrigin (Ray o _) = o
getDirection (Ray _ r) = r

getNormal Sphere p = 
  V4 (p ^._x) (p ^._y) (p ^._z) 0
getNormal Box p
  | 0.5 - x < e  = V4 (signum x) 0 0 0
  | 0.5 - y < e  = V4 0 (signum y) 0 0
  | otherwise    = V4 0 0 (signum z) 0
    where x = abs p ^._x
          y = abs p ^._y
          z = abs p ^._z
          e = 0.000000001

-- TODO r/h fix h = 0
getNormal c@(Cone r1 r2) p = V4 (pre ^._x * (h/r1)) (pre ^._y * (h/r1)) (-(r1 / h)) 0
  where pre = normalize (V3 (p ^._x) (p ^._y) 0)
        h   = coneH c
            
-- TODO fix r1 - r2 = 0
coneH (Cone r1 r2) = r1 / (r1 - r2)

intersection :: Ray -> Primitive -> Maybe Intersection
intersection ray Sphere      = sphereIntersection ray
intersection ray Box         = boxIntersection ray
intersection ray c@(Cone _ _)  = coneIntersection ray c

-- TODO check if open ended
coneIntersection r@(Ray origin direction) cone
  | toSquare < 0 = Nothing
  | otherwise    = nearestConeIntersection r t1 t2 cone
                   where a = dX ** 2 + (dY ** 2) - (dZ **2) 
                         b = 2 * oX * dX + 2 * oY * dY - 2 * oZ * dZ
                         c = oX ** 2 + oY ** 2 - oZ ** 2
                         toSquare = b**2 - 4 * a * c
                         t1 = ((-b) + sqrt toSquare) / (2 * a)
                         t2 = ((-b) - sqrt toSquare)  / (2 * a)
                         (oX, oY, oZ) = (origin ^._x, origin ^._y, origin ^._z)
                         (dX, dY, dZ) = (direction ^._x, direction ^._y, direction ^._z)


nearestConeIntersection r@(Ray o d) t1 t2 c
  | constrain z1 && constrain z2 = Just $ itPoint r $ min t1 t2
  | constrain z1                 = Just $ itPoint r 1
  | constrain z2                 = Just $ itPoint r t2
  | otherwise                    = Nothing
    where z1 = o ^._z + t1 * d ^._z
          z2 = o ^._z + t2 * d ^._z
          constrain f = f >= 0 && f < 1

-- | Calculate the intersection between the ray and the unit box aligned on the
--   axes.
boxIntersection r@(Ray origin direction)
  | tMin > tyMax || tyMin > tMax   = Nothing
  | t2Min > tzMax || tzMin > t2Max = Nothing
  | t1 > 0                         = Just $ itPoint r t1
  | t2 > 0                         = Just $ itPoint r t2
  | otherwise                      = Nothing
    where minB          = -0.5
          maxB          =  0.5
          invDir        = 1 / direction
          (tMin, tMax)  = slabIntersection (minB,maxB) (origin ^._x) (invDir ^._x)
          (tyMin, tyMax)  = slabIntersection (minB,maxB) (origin ^._y) (invDir ^._y)
          (tzMin, tzMax)  = slabIntersection (minB,maxB) (origin ^._z) (invDir ^._z)
          (t2Min, t2Max)  = (max tMin tyMin, min tMax tyMax)
          t1              = max tzMin t2Min
          t2              = min tzMax t2Max
          
-- | Given (min,max) bound range of the axis, origin axis component and 
--   inv direction axis component it calculates the (min,max) factor of a 
--   ray that intersects the slab
slabIntersection (minB,maxB) o invD 
  | invD >= 0 = (calc minB o invD, calc maxB o invD)
  | otherwise = (calc maxB o invD, calc minB o invD)
      where calc b o d = (b - o) * invD

-- | calculates the possible intersectionpoint 
sphereIntersection r@(Ray origin direction) 
  | toSquare < 0 = Nothing
  | otherwise = nearestIntersectionSphere r x y
                where a = dot direction direction
                      b = 2 * (dot direction origin)
                      c = (dot origin origin) - 2
                      toSquare = b**2 - 4 * a * c
                      x = ((-b) + sqrt toSquare) / (2 * a)
                      y = ((-b) - sqrt toSquare)  / (2 * a)

--There can be 2 intersections with a sphere and 
--this function returns the nearest one
nearestIntersectionSphere r res1 res2
  | res1 > 0 && res2 > 0   = Just $ itPoint r $ min res1 res2
  | res1 > 0               = Just $ itPoint r res1
  | res2 > 0               = Just $ itPoint r res2
  | otherwise              = Nothing


itPoint (Ray origin direction) t = origin ^+^ (t *^ direction)
    


