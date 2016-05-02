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
import Linear
import Control.Lens
import Control.Monad

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
  | 0.5 - x < e  = V3 (signum x) 0 0
  | 0.5 - y < e  = V3 0 (signum y) 0
  | otherwise    = V3 0 0 (signum z)
    where x = abs p ^._x
          y = abs p ^._y
          z = abs p ^._z
          e = 0.000000001

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
intersection ray Sphere       = sphereIntersection ray
intersection ray Box          = boxIntersection ray
intersection ray c@(Cone _ _) = coneIntersection ray c
intersection ray (Cylinder rad) = cylinderIntersection ray rad

cylinderIntersection r@(Ray o d) rad = lift f (lift f iC bC) tC 
    where iC = infiniteCylinderIntersection r rad
          bC = capIntersection r (V3 0 0 0) rad
          tC = capIntersection r (V3 0 0 1) rad
          f  = nearestPoint o

-- | Easier intersection function because of our cosntrains to the object
--   Normalized direction vector of cylinder is (0,0,1)
infiniteCylinderIntersection r@(Ray o d) rad =
  case res of
    Just (t1, t2) -> Just $ itPoint r $ min t1 t2
    _             -> Nothing
    where tmp  = v d
          tmp2 = v o
          a    = dot tmp tmp
          b    = 2 * dot tmp tmp2
          c    = (dot tmp2 tmp2) - rad**2 
          v i  = V3 (i ^._x) (i^._y) 0
          res  = quadraticEquation a b c

-- | Given the ray, origin of the cape and 
--  radius of the plane it calculates the intersection point and 
--  returns it if it lies along the ray.
--  Because of the constrains on the primitive the direction vector 
--  of the frustum is (0,0,1).
capIntersection r@(Ray o d) p rad =
  case (planeIntersection r p n) of
    Just x -> capConstrain x p rad 
    _      -> Nothing
    where n = V3 0 0 1

capConstrain q p rad
  | diff ^._z == 0 && dot diff diff < rad ** 2 = Just q
  | otherwise                                  = Nothing 
    where diff = q ^-^ p

-- TODO: do not copy & paste with cylinderIntersection!!
coneIntersection r@(Ray o d) c@(Cone r1 r2) = lift f (lift f iC bC) tC 
    where iC = infinityConeIntersection r c
          bC = capIntersection r (V3 0 0 0) r1
          tC = capIntersection r (V3 0 0 1) r2
          f  = nearestPoint o

infinityConeIntersection r@(Ray o d) (Cone r1 r2) =
  case res of
    Just (t1, t2) -> Just $ itPoint r $ min t1 t2
    _             -> Nothing
  where alpha =  r1 - r2
        pa    = (V3 0 0 r1) ^/ alpha
        diff  = o ^-^ pa
        tmp   = (v d)
        v i   = V3 (i ^._x) (i ^._y) 0
        si    = sin alpha ** 2
        co    = cos alpha ** 2
        a     = co * dot tmp tmp - si * (d ^._z)**2
        b     = 2 * co * dot tmp (v diff) - 2 * si * (d ^._z) * (diff ^._z)
        c     = co * dot (v diff) (v diff) - si * (diff ^._z)**2
        res   = quadraticEquation a b c


lift _ Nothing m = m
lift _ m Nothing = m
lift f (Just m1) (Just m2) = Just (f m1 m2)

-- | Given a ray, point on the plane and the normal vector to the plane
-- return the coefficient if the ray intersects the plane.
planeIntersection r@(Ray o d) p n
  | t <= 0    = Nothing
  | otherwise = Just $ itPoint r t 
     where t = (dot n $ p ^-^ o) / dot n d

-- | Calculate the intersection between the ray and the unit box aligned on the
-- axes.
boxIntersection r@(Ray origin direction)
  | tMax < 0 || tMin > tMax = Nothing
  | tMin > 0                = Just $ itPoint r tMin
  | tMax > 0                = Just $ itPoint r tMax
  | otherwise               = Nothing
  where
    (^/^) = liftU2 (/) -- pointwise division
    slab b = (b - origin) ^/^ direction
    s01 = over both slab (0, 1)
    (vMin, vMax) = over both (\ m -> uncurry (liftU2 m) s01) (min, max)
    tMin = maximum vMin
    tMax = minimum vMax
 
-- | Calculate the possible intersection point.
sphereIntersection r@(Ray origin direction) 
  | toSquare < 0 = Nothing
  | otherwise = itPoint r <$> nearestPositive x y
                where a = dot direction direction
                      b = 2 * (dot direction origin)
                      c = (dot origin origin) - 1
                      toSquare = b**2 - 4 * a * c
                      x = ((-b) + sqrt toSquare) / (2 * a)
                      y = ((-b) - sqrt toSquare)  / (2 * a)

nearestPoint o p1 p2
  | f < s     = p1
  | otherwise = p2
    where f = distance o p1
          s = distance o p2

nearestPositive t1 t2
  | t1 > 0 && t2 > 0   = Just $ min t1 t2
  | t1 > 0             = Just t1 
  | t2 > 0             = Just t2
  | otherwise          = Nothing

quadraticEquation a b c
  | toSquare < 0 = Nothing
  | otherwise    = Just (t1, t2)
    where toSquare = b**2 - 4 * a * c
          t1 = ((-b) + sqrt toSquare) / (2 * a)
          t2 = ((-b) - sqrt toSquare)  / (2 * a)

itPoint (Ray origin direction) t = origin ^+^ (t *^ direction)
