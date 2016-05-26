{-# LANGUAGE TemplateHaskell #-}
module Primitive (
  Primitive(Sphere, Box, Cylinder, Cone),
  Radius,
  Ray(..),
  _o,
  _d,
  getNormal,
  intersection,
  )where

import Linear     hiding (frustum)
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Control.Lens
import Control.Monad
import Control.Applicative

--represents an Euclidean coordinate 
type Radius = Double 
type Intersection = V3 Double

--Types that represent primitives
data Primitive =
    Sphere     --sphere of radius 1 with center in the origin
  | Box        
  | Cone Double Double --Cone with defined base radius and cap radius.
                       --Base at origin, height 1 and aligned positiv
                       --z axis. Base radius > cap radius.
  | Cylinder Double
     deriving(Show)

data Ray = Ray {
 _originR :: V3 Double,
 _directionR :: V3 Double
} deriving Show

makeLenses ''Ray

_o :: Lens' Ray (V3 Double)
_o = originR
_d :: Lens' Ray (V3 Double)
_d = directionR

getNormal :: Primitive -> V3 Double -> V3 Double
getNormal Sphere p =
  V3 (p ^._x) (p ^._y) (p ^._z)
-- TODO fix
getNormal Box p
  | x > y && x > z  = V3 1 0 0
  | y > x && y > z  = V3 0 1 0
  | otherwise       = V3 0 0 1
  where V3 x y z = abs p

-- TODO check
getNormal (Cone r1 r2) p
  | z == 0    = V3 0 0 (-1)
  | z == 1    = V3 0 0 1
  | otherwise = V3 (v ^._x * alpha /r1) (v ^._y * alpha/r1) (r1 / alpha) 
    where alpha =  r1 - r2
          v = normalize $ V3 (p ^._x) (p ^._y) 0
          z = p ^._z

getNormal (Cylinder _) p
  | z == 0    = V3 0 0 (-1)
  | z == 1    = V3 0 0 1
  | otherwise = normalize n
    where n = V3 (p ^._x) (p ^._y) 0
          z = p ^._z

-- TODO: return normal vector
intersection :: Ray -> Primitive -> Maybe Intersection
intersection ray Sphere       = itPoint ray <$> sphereIntersection ray
intersection ray Box          = itPoint ray <$> boxIntersection ray
intersection ray (Cone rad1 rad2) =
  frustumIntersection ray (coneIntersection ray rad1 rad2) rad1 rad2
intersection ray (Cylinder rad) =
  frustumIntersection ray (cylinderIntersection ray rad) rad rad

frustumIntersection :: Ray -> Maybe Double -> Double -> Double -> Maybe (V3 Double)
frustumIntersection r@(Ray o _) t rad1 rad2
  | null l    = Nothing
  | otherwise = Just $ minimumBy (comparing $ distance o) l
    where f  = frustumConstrain r t
          cB = capIntersection r (V3 0 0 0) rad1
          cT = capIntersection r (V3 0 0 1) rad2
          l  = catMaybes [f,cB,cT]
frustumConstrain :: Ray -> Maybe Double -> Maybe (V3 Double)
frustumConstrain r it = do
  p <- itPoint r <$> it
  let z = p ^. _z
  guard (z >= 0.0 && z <= 1.0)
  return p
  
capIntersection :: Ray -> V3 Double -> Double -> Maybe Intersection
capIntersection r o rad = do
  p <- itPoint r <$> planeIntersection r o (V3 0 0 1)
  let q = set _z 0 p
  guard (dot q q <= rad ** 2)
  return p

-- | Easier intersection function because of our cosntrains to the object
--   Normalized direction vector of cylinder is (0,0,1)
cylinderIntersection :: Ray -> Double -> Maybe Double
cylinderIntersection (Ray o d) rad =
  uncurry min <$> solveQuadratic a b c
  where
    vd = v d
    vo = v o
    a  = dot vd vd
    b  = 2 * dot vd vo
    c  = dot vo vo - rad**2
    v  = set _z 0

coneIntersection :: Ray -> Double -> Double -> Maybe Double
coneIntersection (Ray o d) r1 r2 =
  uncurry min <$> solveQuadratic a b c
    where alpha = atan $ r1 - r2
          si = (sin alpha) ** 2
          co = (cos alpha) ** 2
          a = co * (xd ** 2 + yd ** 2) - si * zd ** 2
          b = 2 * co * (xd * xo + yd * yo) - 2 * si * (zo - zd) 
          c = co * (xo ** 2 + yo **2) - si * (zo ** 2 - 2 * zo + 1)
          V3 xo yo zo = o
          V3 xd yd zd = d

-- | Given a ray, point on the plane and the normal vector to the plane
-- return the coefficient if the ray intersects the plane.
planeIntersection :: Ray -> V3 Double -> V3 Double -> Maybe Double
planeIntersection (Ray o d) p n
  | t <= 0    = Nothing
  | otherwise = Just t
  where t = dot n (p ^-^ o) / dot n d

-- | Calculate the intersection between the ray and the unit box aligned on the
-- axes.
boxIntersection :: Ray -> Maybe Double
boxIntersection (Ray origin direction)
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

slabIntersection :: (Num a, Ord a) => s -> s -> ((a -> Const a a) -> s -> Const a s) -> (a, a)
slabIntersection o i l
  | i ^. l >= 0 = over both slab (0,1)
  | otherwise   = over both slab (1,0)
  where
    slab b = (b - (o ^. l)) * (i ^. l)

-- | Calculate the possible intersection point.
sphereIntersection :: Ray -> Maybe Double
sphereIntersection (Ray origin direction) =
  solveQuadratic a b c >>= uncurry nearestPositive
  where
    a = dot direction direction
    b = 2 * dot direction origin
    c = dot origin origin - 1

nearestPositive :: (Fractional a, Num a, Ord a) => a -> a -> Maybe a
nearestPositive t1 t2
  | t1 > 0 && t2 > 0 = Just $ min t1 t2
  | t1 > 0           = Just t1
  | t2 > 0           = Just t2
  | otherwise        = Nothing

solveQuadratic :: (Fractional a, Floating a, Ord a) => a -> a -> a -> Maybe (a, a)
solveQuadratic a b c
  | toSquare < 0 = Nothing
  | otherwise    = Just (t1, t2)
  where
    toSquare = b**2 - 4 * a * c
    t1 = ((-b) + sqrt toSquare) / (2 * a)
    t2 = ((-b) - sqrt toSquare) / (2 * a)

itPoint :: Ray -> Double -> V3 Double
itPoint (Ray origin direction) t = origin ^+^ (t *^ direction)
