module Object where

import Linear
import Primitive
import Colour
import Data.Monoid
import Control.Lens

-------------------------------------------------------------------------------
--
--                      Types representing a scene object
--
-------------------------------------------------------------------------------


data Object = Object {
  prim :: Primitive,
  oModifier :: OModifier
}


data OModifier = OModifier {
  transMatrix :: Last (M44 Double),
  texture :: Texture
}


data Texture = Texture {
  pigment :: (Last Colour),
  propert :: TPropertie
  } deriving (Show, Eq)

data TPropertie = TPropertie {
  tAmbient   :: Last Double,
  tDiffuse   :: Last Double,
  tSpecular  :: Last Double,
  tRoughness :: Last Double
  } deriving (Show, Eq)

instance Monoid Texture where
  mempty = Texture mempty mempty
  mappend (Texture c1 f1) (Texture c2 f2) =
    Texture (mappend c1 c2) (mappend f1 f2)

instance Monoid TPropertie where
  mempty = TPropertie mempty mempty mempty mempty
  mappend (TPropertie a1 d1 s1 r1) (TPropertie a2 d2 s2 r2) =
    TPropertie (mappend a1 a2) (mappend d1 d2) (mappend s1 s2) (mappend r1 r2)

instance Monoid OModifier where
  mempty = OModifier mempty mempty
  mappend (OModifier tr1 t1) (OModifier tr2 t2) = 
   OModifier (mappend tr1 tr2) (mappend t1 t2)


-------------------------------------------------------------------------------
--
--                    Intersection functions
--
-------------------------------------------------------------------------------

-- | Applying transformation to ray, that will simulate transformation on 
-- primitive. So some intersection functions stay simple.
transformRay :: Ray -> M44 Double -> Ray
transformRay (Ray o d) trans =
  Ray newOrigin newDirection
  where newOrigin    = trans !* o
        newDirection = trans !* d

-- TODO Box is handelt different then Cones and spheres
-- ASK prof for solution
-- | Intersect ray with object and return ray, object and
-- distance from ray origin to object intersection.
rayObjectIntersection :: Ray -> Object -> Maybe (Ray, Object,V4 Double, Double)
rayObjectIntersection ray o@(Object p om) = do
  mat <- getLast $ transMatrix om
  res <- intersection (transformRay ray $ inv44 mat) p
  return (ray, o, normalVector (mat !* res) mat,getOrigin ray `distance` res)


------------------------------------------------------------------------------
--
--                    Utility functions
--
------------------------------------------------------------------------------

-- TODO Test this function
-- Same problem as rayObjectIntersection funktion, this does not work for boxes
normalVector :: V4 Double -> M44 Double -> V4 Double
normalVector p tr = rot !*! invScale !*! invScale !* p
  where invScale  = V4 (V4 (1 / scaleX tr) 0 0 0) (V4 0 (1 / scaleY tr) 0 0)
                       (V4 0 0 (1 / scaleZ tr) 0) (V4 0 0 0 1)
        rot       = V4 (tr ^._x) (tr ^._y) (tr ^._z) (V4 0 0 0 1)

scaleX tr = sqrt $ dot c c 
  where c = tr ^._x

scaleY tr = sqrt $ dot c c 
  where c = tr ^._y

scaleZ tr = sqrt $ dot c c 
  where c = tr ^._z

getColour :: Object -> Colour
getColour (Object _ p) = case getLast . pigment $ texture p of
  Nothing -> Colour 0 0 0
  Just x -> x
