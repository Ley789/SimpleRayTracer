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
type Intersection = V4 Double

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

getNormal :: Primitive -> V3 Double -> V4 Double
getNormal Sphere p = vector p

-- Epsilon is neede because double cannot represent all real numbers
-- It could be changed if the intersection functons return the normal
-- and the box intersection is changed to intersect between planes
getNormal Box p
  | x > 0 && y > 0 && z <= 0 + e = V4 0 0 1 0
  | x > 0 && y > 0 && z <= 1 - e = V4 0 0 (-1) 0
  | y > 0 && z > 0 && x <= 0 + e = V4 (-1) 0 0 0
  | y > 0 && z > 0 && x <= 1 - e = V4 1 0 0 0
  | x > 0 && z > 0 && y <= 0 + e = V4 0 (-1) 0 0
  | otherwise = V4 0 0 1 0
  where V3 x y z = p
        e = 0.0001

getNormal (Cone r1 r2) p
  | pointInPlane p (V3 0 0 0) n  = V4 0 0 (-1) 0
  | pointInPlane p (V3 0 0 1) n  = V4 0 0 1 0
  | otherwise = V4 xn yn (r1 / c) 0
    where h =  r1 / (r1 - r2)
          c = sqrt $ h ** 2 + r1 ** 2
          n = V3 0 0 1
          V3 x y _ = p
          m = sqrt $ x**2 + y**2
          xn = (x / m) * (h / c)
          yn = (y / m) * (h / c) 

getNormal (Cylinder _) p
  | pointInPlane p (V3 0 0 0) n  = V4 0 0 (-1) 0
  | pointInPlane p (V3 0 0 1) n  = V4 0 0 1 0
  | otherwise =  V4 (p ^._x) (p ^._y) 0 0
    where n  = V3 0 0 1


intersection :: Ray -> Primitive -> Maybe Intersection
intersection (Ray o d) = intersectionf (EuclideanRay (o ^._xyz) (d ^._xyz))

intersectionf :: EuclideanRay -> Primitive -> Maybe Intersection
intersectionf ray Sphere       = point <$> itPoint ray <$> sphereIntersection ray
intersectionf ray Box          = point <$> itPoint ray <$> boxIntersection ray
intersectionf ray (Cone rad1 rad2) = point <$>
  frustumIntersection ray (coneIntersection ray rad1 rad2) rad1 rad2
intersectionf ray (Cylinder rad) = point <$>
  frustumIntersection ray (cylinderIntersection ray rad) rad rad

frustumIntersection :: EuclideanRay -> Maybe Double -> Double -> Double -> Maybe (V3 Double)
frustumIntersection r@(EuclideanRay o _) t rad1 rad2
  | null l    = Nothing
  | otherwise = Just $ minimumBy (comparing $ distance o) l
    where f  = frustumConstrain r t
          cB = capIntersection r (V3 0 0 0) rad1
          cT = capIntersection r (V3 0 0 1) rad2
          l  = catMaybes [f,cB,cT]

frustumConstrain :: EuclideanRay -> Maybe Double -> Maybe (V3 Double)
frustumConstrain r it = do
  p <- itPoint r <$> it
  let z = p ^. _z
  guard (z >= 0.0 && z <= 1.0)
  return p
  
capIntersection :: EuclideanRay -> V3 Double -> Double -> Maybe (V3 Double)
capIntersection r o rad = do
  p <- itPoint r <$> planeIntersection r o (V3 0 0 1)
  let q = set _z 0 p
  guard (dot q q  <= rad ** 2)
  return p

-- | Easier intersection function because of our cosntrains to the object
--   Normalized direction vector of cylinder is (0,0,1)
cylinderIntersection :: EuclideanRay -> Double -> Maybe Double
cylinderIntersection (EuclideanRay o d) rad =
  uncurry min <$> solveQuadratic a b c
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
          va = V3 0 0 1
          deltaP = o ^-^va
          tmpA = d - (dot d va) *^va
          tmpC = deltaP - (dot deltaP va) *^va
          a = co * (dot tmpA tmpA) - si * (dot d va) ** 2
          b = 2 * co * (dot tmpA (deltaP - (dot deltaP va) *^ va)) - 2 * si * (dot d va) * (dot deltaP va)
          c = co * (dot tmpC tmpC) - si * (dot deltaP va)
         --  a = co * (xd ** 2 + yd ** 2) - si * zd ** 2
         -- b = 2 * co * (xd * xo + yd * yo) - 2 * si * (zo - zd) 
         --  c = co * (xo ** 2 + yo **2) - si * (zo ** 2 - 1) ** 2
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
boxIntersection :: EuclideanRay -> Maybe Double
boxIntersection (EuclideanRay origin direction)
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
sphereIntersection :: EuclideanRay -> Maybe Double
sphereIntersection (EuclideanRay origin direction) =
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

itPoint :: EuclideanRay -> Double -> V3 Double
itPoint (EuclideanRay origin direction) t = origin ^+^ (t *^ direction)

-- shares the same problem as getNormal of box
-- | Given a point p, a point on the plane q and the normal vector to 
-- the plane n, returns true if p lies in the plane 
pointInPlane :: V3 Double -> V3 Double -> V3 Double -> Bool
pointInPlane q p n = r <= e && r >= (-e)  
                 where r = dot (q - p) n
                       e = 0.000001
