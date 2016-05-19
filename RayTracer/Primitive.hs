module Primitive (
  Primitive(Sphere, Box, Cylinder, Cone),
  Radius,
  Ray(..),
  origin,
  direction,
  getOrigin,
  getDirection,
  getNormal,
  intersection,
  )where

import qualified Debug.Trace as T 
import Linear     hiding (frustum)
import Data.List
import Data.Maybe (fromJust,catMaybes)
import Control.Lens
import Control.Monad
import Control.Applicative
--represents an Euclidean coordinate 
type Center = V3 Double
type Origin = V3 Double
type Direction = V3 Double
type Radius = Double 
type Intersection = V3 Double
        
--Types that represent primitives
data Primitive =
    Sphere     --sphere of radius 1 with center in the origin
  | Box        
  | Cone {            --Cone with defined base radius and cap radius.
    baseR :: Double,  --Base at origin, height 1 and aligned positive
    capR  :: Double   --z axis. Base radius > cap radius.
  }
  | Cylinder Double
     deriving(Show)

data Ray = Ray Origin Direction
           deriving Show

makeCone r1 r2
  | r1 > r2   = Cone r1 r2
  | otherwise = Cone r1 r1

origin :: Double -> Double -> Double -> Origin
origin x y z = V3 x y z

direction :: Double -> Double -> Double -> Direction
direction x y z = V3 x y z

ray :: Origin -> Direction -> Ray
ray o d = Ray o d

getOrigin (Ray o _) = o
getDirection (Ray _ r) = r

getNormal Sphere p =
  V3 (p ^._x) (p ^._y) (p ^._z)
getNormal Box p
  | 1 - x < e  = V3 1 0 0
  | 1 - y < e  = V3 0 1 0
  | otherwise  = V3 0 0 1
    where x = abs p ^._x
          y = abs p ^._y
          z = abs p ^._z
          e = 0.00000001
-- TODO check
getNormal c@(Cone r1 r2) p
  | z == 0    = V3 0 0 (-1)
  | z == 1    = V3 0 0 1
  | otherwise = V3 (v ^._x * alpha /r1) (v ^._y * alpha/r1) (r1 / alpha) 
    where alpha =  r1 - r2
          v = normalize $ V3 (p ^._x) (p ^._y) 0
          z = p ^._z

getNormal c@(Cylinder r) p
  | z == 0    = V3 0 0 (-1)
  | z == 1    = V3 0 0 1
  | otherwise = normalize n
    where n = V3 (p ^._x) (p ^._y) 0
          z = p ^._z

intersection :: Ray -> Primitive -> Maybe Intersection
intersection ray Sphere       = itPoint ray <$> sphereIntersection ray
intersection ray Box          = itPoint ray <$> boxIntersection ray
intersection ray (Cone rad1 rad2) =
  frustumIntersection ray (coneIntersection ray rad1 rad2) rad1 rad2
intersection ray (Cylinder rad) =
  frustumIntersection ray (cylinderIntersection ray rad) rad rad

frustumIntersection r@(Ray o d) it rad1 rad2
  | l == []  = Nothing
  | otherwise = Just $ foldl1' (nearestPoint o) l 
    where f  = frustum r it
          cB = capIntersection r (V3 0 0 0) rad1
          cT = capIntersection r (V3 0 0 1) rad2
          l  = catMaybes [f,cB,cT]

frustum r it = frustumConstrain p
  where p = itPoint r <$> it

frustumConstrain p
  | p == Nothing         = Nothing
  | z >= 0.0 && z <= 1.0 = p
  | otherwise            = Nothing
  where z = (fromJust p) ^. _z 

capIntersection :: Ray -> V3 Double -> Double -> Maybe Intersection
capIntersection r o rad
  | p == Nothing        = Nothing
  | dot q q <= rad ** 2 = p
  | otherwise           = Nothing 
  where p = itPoint r <$> planeIntersection r o (V3 0 0 1)
        q = set _z 0 (fromJust p)

-- | Easier intersection function because of our cosntrains to the object
--   Normalized direction vector of cylinder is (0,0,1)
cylinderIntersection r@(Ray o d) rad =
  uncurry min <$> solveQuadratic a b c
  where
    vd = v d
    vo = v o
    a  = dot vd vd
    b  = 2 * dot vd vo
    c  = dot vo vo - rad**2
    v  = set _z 0

coneIntersection r@(Ray o d) r1 r2 =
  uncurry min <$> solveQuadratic a b c
  where alpha = r1 - r2
        pa    = (V3 0 0 r1) ^/ alpha
        diff  = o ^-^ pa
        vd    = v d
        vdiff = v diff
        v     = set _z 0
        si    = sin alpha ** 2
        co    = cos alpha ** 2
        a     = co * dot vd vd - si * (d ^._z)**2
        b     = 2 * co * dot vd vdiff - 2 * si * (d ^._z) * (diff ^._z)
        c     = co * dot vdiff vdiff - si * (diff ^._z)**2

-- | Given a ray, point on the plane and the normal vector to the plane
-- return the coefficient if the ray intersects the plane.
planeIntersection r@(Ray o d) p n
  | t <= 0    = Nothing
  | otherwise = Just t
  where t = (dot n $ p ^-^ o) / dot n d

-- | Calculate the intersection between the ray and the unit box aligned on the
-- axes.
boxIntersection :: Ray -> Maybe Double
boxIntersection r@(Ray origin direction)
  | tMax < 0    = Nothing
  | tMin > tMax = Nothing
  | tMin > 0    = Just tMin
  | tMax > 0    = Just tMax
  | otherwise   = Nothing
  where
    invD = fmap (1/) direction
    ts = map (slabIntersection origin invD) [_x , _y, _z]
    tMin = maximum $ map fst ts
    tMax = minimum $ map snd ts

slabIntersection o i l
  | i ^. l >= 0 = over both slab (0,1)
  | otherwise   = over both slab (1,0)
  where
    slab b = (b - (o ^. l)) * (i ^. l)

-- | Calculate the possible intersection point.
sphereIntersection r@(Ray origin direction) =
  solveQuadratic a b c >>= uncurry nearestPositive
  where
    a = dot direction direction
    b = 2 * dot direction origin
    c = dot origin origin - 1

nearestPoint o p1 p2
  | distance o p1 < distance o p2 = p1
  | otherwise = p2

nearestPositive t1 t2
  | t1 > 0 && t2 > 0 = Just $ min t1 t2
  | t1 > 0           = Just t1
  | t2 > 0           = Just t2
  | otherwise        = Nothing

solveQuadratic a b c
  | toSquare < 0 = Nothing
  | otherwise    = Just (t1, t2)
  where
    toSquare = b**2 - 4 * a * c
    t1 = ((-b) + sqrt toSquare) / (2 * a)
    t2 = ((-b) - sqrt toSquare) / (2 * a)

itPoint (Ray origin direction) t = origin ^+^ (t *^ direction)
