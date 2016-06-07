{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Primitive (
  Primitive(Sphere, Box, Cylinder, Cone),
  Radius,
  Ray(..),
  _o,
  _d,
  intersection,
  )where

import Linear     hiding (frustum)
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord   (comparing)
import Control.Lens
import Control.Monad
import Control.Applicative


--represents an Euclidean coordinate 
type Radius = Double 

--Types that represent primitives
data Primitive =
    Sphere     --sphere of radius 1 with center in the origin
  | Box        
  | Cone Double Double --Cone with defined base radius and cap radius.
                       --Base at origin, height 1 and aligned positiv
                       --z axis. Base radius > cap radius.
  | Cylinder Double
     deriving(Show)

-- | Convention: 4-th value of originR is always 1
data Ray = Ray {
 _originR :: V4 Double,
 _directionR :: V4 Double
} deriving Show

-- | Intern reperesentation of ray, because intersection function are
-- based on equation in euclidean space
data EuclideanRay = EuclideanRay  (V3 Double) (V3 Double)
  deriving Show

makeLenses ''Ray

_o :: Lens' Ray (V4 Double)
_o = originR
_d :: Lens' Ray (V4 Double)
_d = directionR


normalCone :: Double -> Double -> V3 Double -> V3 Double
normalCone r1 r2 p = V3 xn yn (r1 / c)
    where h =  r1 / (r1 - r2)
          c = sqrt $ h ** 2 + r1 ** 2
          V3 x y _ = p
          m = sqrt $ x**2 + y**2
          xn = (x / m) * (h / c)
          yn = (y / m) * (h / c) 

normalCylinder :: V3 Double -> V3 Double
normalCylinder p = normalize $  V3 (p ^._x) (p ^._y) 0

intersection :: Ray -> Primitive -> Maybe (V4 Double, V4 Double)
intersection (Ray o d) = intersectionf (EuclideanRay (o ^._xyz) (d ^._xyz))

intersectionf :: EuclideanRay -> Primitive -> Maybe (V4 Double, V4 Double)
intersectionf ray Sphere = transformToHom <$> (\x -> (x,x)) <$> sphereIntersection ray
intersectionf ray Box = transformToHom <$> boxIntersection ray
intersectionf ray (Cone rad1 rad2) = transformToHom <$>
  frustumIntersection ray (coneIntersection ray rad1 rad2) rad1 rad2 (normalCone rad1 rad2)
intersectionf ray (Cylinder rad) = transformToHom <$>
  frustumIntersection ray (cylinderIntersection ray rad) rad rad normalCylinder

transformToHom :: (V3 Double, V3 Double) -> (V4 Double, V4 Double)
transformToHom (x,y) = (point x, vector y)

frustumIntersection :: EuclideanRay -> Maybe Double -> Double -> Double -> (V3 Double -> V3 Double) -> Maybe (V3 Double, V3 Double)
frustumIntersection r@(EuclideanRay o _) t rad1 rad2 n
  | null l    = Nothing
  | otherwise = Just $ minimumBy (comparing $ distance o . fst) l
    where f  = frustumConstrain r t n
          cB = capIntersection r (V3 0 0 0) (-1) rad1
          cT = capIntersection r (V3 0 0 1)  1 rad2
          l  = catMaybes [f,cB,cT]

frustumConstrain :: EuclideanRay -> Maybe Double -> (V3 Double -> V3 Double) -> Maybe (V3 Double, V3 Double)
frustumConstrain r it n = do
  p <- itPoint r <$> it
  let z = p ^. _z
  guard (z >= 0 && z <= 1)
  return (p,n p)

capIntersection :: EuclideanRay -> V3 Double -> Double -> Double -> Maybe (V3 Double, V3 Double)
capIntersection r o d rad = do
  p <- itPoint r <$> planeIntersection r o (V3 0 0 1)
  let q = set _z 0 p
  guard (dot q q  <= rad ** 2)
  return (p, V3 0 0 d)

-- | Easier intersection function because of our cosntrains to the object
--   Normalized direction vector of cylinder is (0,0,1)
cylinderIntersection :: EuclideanRay -> Double -> Maybe Double
cylinderIntersection (EuclideanRay o d) rad =
  solveQuadratic a b c >>= uncurry nearestPositive
  where
    vd = v d
    vo = v o
    a  = dot vd vd
    b  = 2 * dot vd vo
    c  = dot vo vo - rad**2
    v  = set _z 0

coneIntersection :: EuclideanRay -> Double -> Double -> Maybe Double
coneIntersection (EuclideanRay o d) r1 r2 =
  solveQuadratic a b c >>= uncurry nearestPositive
    where alpha = atan $ r1 - r2
          si = sin alpha ** 2
          co = cos alpha ** 2
          a = co * (xd ** 2 + yd ** 2) - si * zd ** 2
          b = 2 * co * (xd * xo + yd * yo) - 2 * si * zd * (zo - rc) 
          c = co * (xo ** 2 + yo **2) - si * (zo - rc) ** 2
          rc = r1 / (r1 - r2)
          V3 xo yo zo = o
          V3 xd yd zd = d

-- | Given a ray, point on the plane and the normal vector to the plane
-- return the coefficient if the ray intersects the plane.
planeIntersection :: EuclideanRay -> V3 Double -> V3 Double -> Maybe Double
planeIntersection (EuclideanRay o d) p n
  | t <= 0    = Nothing
  | otherwise = Just t
  where t = dot n (p ^-^ o) / dot n d

-- | Calculate the intersection between the ray and the unit box aligned on the
-- axes.
boxIntersection :: EuclideanRay -> Maybe (V3 Double, V3 Double)
boxIntersection r@(EuclideanRay origin direction)
  | tMax < 0    = Nothing
  | tMin > tMax = Nothing
  | tMin > 0    = Just (itPoint r tMin, snd minL)
  | tMax > 0    = Just (itPoint r tMax, negate $ snd minL)
  | otherwise   = Nothing
  where
    invD = fmap (1/) direction
    a = slabIntersection origin invD
-- ghc type error if ts = map a [_x, _y, _z] is used
    ts = [a _x, a _y, a _z]
    minL = maximumBy (comparing extMin) ts
    maxL = minimumBy (comparing extMax) ts 
    extMin = fst . fst
    extMax = snd . fst
    tMin = extMin minL
    tMax = extMax maxL

-- slab returns the coefficents needed to reach the bounds in the corresponding
-- axis and the normal vector direction of the min value.
slabIntersection :: V3 Double -> V3 Double -> Lens' (V3 Double) Double -> ((Double, Double), V3 Double)
slabIntersection o i l
  | i ^. l >= 0 = (over both slab (0,1), normalD (-1))
  | otherwise   = (over both slab (1,0), normalD 1)
  where
    slab b = (b - (o ^. l)) * (i ^. l)
    normalD v = set l v (V3 0 0 0)

-- | Calculate the possible intersection point.
sphereIntersection :: EuclideanRay -> Maybe (V3 Double)
sphereIntersection r@(EuclideanRay origin direction) =
  itPoint r <$> (solveQuadratic a b c >>= uncurry nearestPositive)
  where
    a = dot direction direction
    b = dot ( 2 *^ direction) origin
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

itPoint :: EuclideanRay -> Double -> V3 Double
itPoint (EuclideanRay origin direction) t = origin ^+^ (t *^ direction)