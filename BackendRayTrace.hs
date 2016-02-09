{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module BackendRayTrace where

import           Control.Lens                   (view)
import           Data.Monoid                    (Last (..))
import           Data.Tree
import           Data.Typeable
import           Data.List                      (last)

import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude               as D hiding (Last (..), view)
import           Scene
import qualified Primitives                     as P
import           Material
import           Colour
import           Linear
data Ray = Ray
     deriving (Eq,Ord,Read,Show,Typeable)

type instance V Ray = V3
type instance N Ray = Double

instance Monoid (Render Ray V3 Double) where
   mempty = MRay []
   (MRay f) `mappend`(MRay s) = MRay (f ++ s)

instance Backend Ray V3 Double where
   data Render Ray V3 Double = MRay [SceneObject]
   type Result Ray V3 Double = Scene
   data Options Ray V3 Double = RayOptions
  
   renderRTree _ _ rt = go $ rt where
     go :: RTree Ray V3 Double a -> Scene
     go (Node (RPrim p) _)   = unRay $ render Ray p
     --we ignore textures atm
     go (Node (RStyle s) ts) = concatMap go $ ts
     go (Node _ ts)          = concatMap go $ ts

unRay :: Render Ray V3 Double -> Scene
unRay (MRay x) = x


class ToObject t where
      toObject :: t -> SceneObject


-- change to use matrixHomeRep for more flexibility and change to use V3
-- atm standard color is red
instance ToObject (Ellipsoid Double) where
  toObject (Ellipsoid t) = rayPrimSphere t


rayPrimSphere t = Object P.Sphere (rayTrans t)

rayTrans :: T3 Double -> RayModifier
rayTrans t = RM (getTransformation $ inv $ t)                    

getTransformation = listToMatrix . matrixHomRep 
--converts List of 3*3 elements to 4x4 homogenious matrix
listToMatrix :: (Num a) => [[a]] -> M44 a
listToMatrix (x:y:z:w:_) = transpose $ V4 (homVector x 0) (homVector y 0) 
                                          (homVector z 0) (homVector w 1)

homVector (x:y:z:_) c = V4 x y z c

instance Renderable (Ellipsoid Double) Ray where
      render _ = wrapSolid . toObject

wrapSolid :: SceneObject -> Render Ray V3 Double
wrapSolid = MRay . (:[]) 
