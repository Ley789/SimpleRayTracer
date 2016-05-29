{-# LANGUAGE TemplateHaskell #-}

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

data Matrices = Matrices {
  _transM :: M44 Double,
  _invM   :: M44 Double,
  _normM  :: M44 Double
} deriving(Show)

data Object = Object {
  _prim      :: Primitive,
  _oModifier :: OModifier
} deriving(Show)

data OModifier = OModifier {
  _matrices :: Matrices,
  _texture  :: Texture
} deriving(Show)

data Texture = Texture {
  _pigment  ::  Colour,
  _property :: TProperty
} deriving (Show, Eq)

data TProperty = TProperty {
  _tAmbient   :: Double,
  _tDiffuse   :: Double,
  _tSpecular  :: Double,
  _tRoughness :: Double
} deriving (Show, Eq)

data Intersection = Intersection {
  _ray     :: Ray, 
  _itTex   :: Texture,
  _normal  :: V4 Double, 
  _itPoint :: V4 Double
} deriving (Show)

makeLenses ''Object
makeLenses ''OModifier
makeLenses ''Texture
makeLenses ''TProperty
makeLenses ''Intersection
makeLenses ''Matrices

instance Monoid Texture where
  mempty = Texture (Colour 0 0 0) mempty
  mappend (Texture c1 f1) (Texture c2 f2) =
    Texture (c1 + c2) (mappend f1 f2)

instance Monoid TProperty where
  mempty = TProperty 0 0 0 0
  mappend (TProperty a1 d1 s1 r1) (TProperty a2 d2 s2 r2) =
    TProperty (a1 + a2) (d1 + d2) (s1 + s2) (r1 + r2)

-------------------------------------------------------------------------------
--
--                    Intersection functions
--
-------------------------------------------------------------------------------


-- | Applying transformation to ray, that will simulate transformation on 
-- primitive. So some intersection functions stay simple.
transformRay :: Ray -> M44 Double -> Ray
transformRay (Ray o d) trans =
  Ray (newOrigin ^* normO) newDirection
  where newOrigin    = trans !* o
        newDirection = trans !* d
        normO        = 1 / (newOrigin ^._w)

-- TODO Check functionality with translaten in the matrix!!
-- | Intersect ray with object and return ray, object, normal vector and
-- point of the intersection.
rayObjectIntersection :: Ray -> Object -> Maybe Intersection
rayObjectIntersection r o@(Object p om) = do
  let nr = transformRay r $ om ^. matrices . invM
  res <- intersection nr p
  let n = normalVector (om ^. matrices) $ getNormal p (normalizePoint res)
  let pos = om ^. matrices . transM !* res
  return $ Intersection r (o ^. oModifier . texture) n pos


------------------------------------------------------------------------------
--
--                    Utility functions
--
------------------------------------------------------------------------------

-- Convention of ray is w =1 so we only need to drop components
rayPointDistance :: Ray -> V4 Double -> Double
rayPointDistance r p = distance (r ^._o . _xyz) (p ^._xyz)

normalVector :: Matrices -> V4 Double -> V4 Double
normalVector m p =  normalize $ m ^. normM !* p

-- TODO Test
rot44 :: M44 Double -> M44 Double -> M44 Double
rot44 s t = r !*! s !*! s
  where r = dropTrans t

invScale44 :: M44 Double -> M44 Double
invScale44 t =
  V4 (V4 (1 / scale t _x) 0 0 0)
     (V4 0 (1 / scale t _y) 0 0)
     (V4 0 0 (1 / scale t _z) 0)
     (V4 0 0 0 1)


scale tr l = dot c c
  where c  = fmap (view l) (view _xyz tr)

-- | Set translation to (0, 0, 0).
dropTrans :: M44 Double -> M44 Double
dropTrans = over _xyz (fmap (set _w 0))

oProp :: Lens' Object TProperty
oProp = oModifier . texture . property


-------------------------------------------------------------------------------
-- Control for MatSet data type
-------------------------------------------------------------------------------

matricesOfM44 :: M44 Double -> Matrices
matricesOfM44 m = Matrices m (inv44 m) (rot44 (invScale44 m) m)

