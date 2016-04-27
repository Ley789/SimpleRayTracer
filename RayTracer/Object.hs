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
newtype MatSet = MatSet {
  _matrixes :: Matrixes
} deriving(Show)

-- intern reprensentation
-- will not be exported to ensure consistency
data Matrixes = Matrixes { 
  _transformationM   :: M44 Double,
  _invertMatrix      :: M44 Double,
  _normalTMatrix     :: M44 Double
} deriving(Show)

data Object = Object {
  _prim      :: Primitive,
  _oModifier :: OModifier
} deriving(Show)

data OModifier = OModifier {
  _mSet    :: MatSet,
  _texture :: Texture
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
  _object  :: Object,
  _normal  :: V4 Double, 
  _itPoint :: V4 Double
} deriving (Show)

makeLenses ''Object
makeLenses ''OModifier
makeLenses ''Texture
makeLenses ''TProperty
makeLenses ''Intersection
makeLenses ''MatSet
makeLenses ''Matrixes

instance Monoid MatSet where
  mempty = MatSet (Matrixes (identity :: M44 Double) (identity :: M44 Double) 
                            (identity :: M44 Double))
  mappend m1 m2 = createMapSet (t1 !+! t2)
    where t1 = m1 ^. tMat
          t2 = m2 ^. tMat
instance Monoid Texture where
  mempty = Texture (Colour 0 0 0) mempty
  mappend (Texture c1 f1) (Texture c2 f2) =
    Texture (c1 + c2) (mappend f1 f2)

instance Monoid TProperty where
  mempty = TProperty 0 0 0 0
  mappend (TProperty a1 d1 s1 r1) (TProperty a2 d2 s2 r2) =
    TProperty (a1 + a2) (d1 + d2) (s1 + s2) (r1 + r2)

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
  where newOrigin    = (trans !* point o) ^._xyz
        newDirection = (trans !* vector d) ^._xyz

-- TODO Check functionality with translaten in the matrix!!
-- | Intersect ray with object and return ray, object, normal vector and
-- point of the intersection.
rayObjectIntersection :: Ray -> Object -> Maybe Intersection
rayObjectIntersection ray o@(Object p om) = do
  res <- intersection (transformRay ray $ om ^. mSet . invTMat) p
  return $ Intersection ray  o 
           (normalVector (om ^. mSet) $ point $ getNormal p res) 
           (om ^. mSet . tMat !* point res)

------------------------------------------------------------------------------
--
--                    Utility functions
--
------------------------------------------------------------------------------

rayPointDistance ray p = distance (getOrigin ray) (normalizePoint p)

normalVector :: MatSet -> V4 Double -> V4 Double
normalVector m p =  m ^. norMat !* p
       
-- TODO Test
rot44 s t = r !*! s !*! s
  where r = dropTrans t

-- TODO Test
invScale44 :: M44 Double -> M44 Double
invScale44 t = 
  V4 (V4 (1 / scale tr _x) 0 0 0) 
     (V4 0 (1 / scale tr _y) 0 0)
     (V4 0 0 (1 / scale tr _z) 0) 
     (V4 0 0 0 1)
   where tr = dropTrans t
  
scale tr l = sqrt $ dot c c 
  where c = tr ^. l

dropTrans :: M44 Double -> M44 Double
dropTrans = (dropComp _x) . (dropComp _y) . (dropComp _z)
  where dropComp lens = over (lens . _w) (\_ -> 0)

getColour :: Object -> Colour
getColour o = o ^. oModifier . texture . pigment

getProperty o l = o ^. oModifier . texture . property . l


-------------------------------------------------------------------------------
-- Control for MatSet data type
-------------------------------------------------------------------------------

tMat = matrixes . transformationM
invTMat = matrixes . invertMatrix
norMat = matrixes . normalTMatrix

createMapSet :: M44 Double -> MatSet
createMapSet m = MatSet (Matrixes m (inv44 m) (rot44 i m))
  where i = invScale44 m
