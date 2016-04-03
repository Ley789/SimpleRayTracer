{-# LANGUAGE TemplateHaskell #-}
module Object where

import Linear
import Primitive
import Colour
import Data.Maybe
import Data.Monoid
import Control.Lens

-------------------------------------------------------------------------------
--
--                      Types representing a scene object
--
-------------------------------------------------------------------------------

data Object = Object {
  _prim :: Primitive,
  _oModifier :: OModifier
} deriving(Show)

data OModifier = OModifier {
  _transMatrix :: Last (M44 Double),
  _texture :: Texture
} deriving(Show)

data Texture = Texture {
  _pigment :: Last Colour,
  _property :: TProperty
} deriving (Show, Eq)

data TProperty = TProperty {
  _tAmbient   :: Last Double,
  _tDiffuse   :: Last Double,
  _tSpecular  :: Last Double,
  _tRoughness :: Last Double
} deriving (Show, Eq)

data Intersection = Intersection {
  _ray :: Ray, 
  _object:: Object,
  _normal :: V4 Double, 
  _itPoint :: V4 Double
} deriving (Show)

makeLenses ''Object
makeLenses ''OModifier
makeLenses ''Texture
makeLenses ''TProperty
makeLenses ''Intersection

instance Monoid Texture where
  mempty = Texture mempty mempty
  mappend (Texture c1 f1) (Texture c2 f2) =
    Texture (mappend c1 c2) (mappend f1 f2)

instance Monoid TProperty where
  mempty = TProperty mempty mempty mempty mempty
  mappend (TProperty a1 d1 s1 r1) (TProperty a2 d2 s2 r2) =
    TProperty (mappend a1 a2) (mappend d1 d2) (mappend s1 s2) (mappend r1 r2)

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

-- TODO Check functionality with translaten in the matrix!!
-- | Intersect ray with object and return ray, object, normal vector and
-- point of the intersection.
rayObjectIntersection :: Ray -> Object -> Maybe Intersection
rayObjectIntersection ray o@(Object p om) = do
  mat <- getLast $ _transMatrix om
  res <- intersection (transformRay ray $ inv44 mat) p
  return $ Intersection ray  o (normalVector mat $ getNormal p res) (mat !* res)


------------------------------------------------------------------------------
--
--                    Utility functions
--
------------------------------------------------------------------------------

rayPointDistance ray p = distance (normalizePoint $ getOrigin ray) (normalizePoint p)

-- TODO Test this function
-- Same problem as rayObjectIntersection funktion, this does not work for boxes
normalVector :: M44 Double -> V4 Double -> V4 Double
normalVector tr p = normalize $ rot !*! invScale !*! invScale !* p
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
getColour (Object Nil _) = Colour 0 0 0
getColour (Object _ p) = fromMaybe (Colour 1 1 1) (getLast . _pigment $ _texture p)

getProperty o l = fromMaybe 0 (getLast $ o ^. oModifier . texture . property . l)

setObjectModifier :: Object -> OModifier -> Object
setObjectModifier o m = set oModifier (mappend (o ^. oModifier) m) o
