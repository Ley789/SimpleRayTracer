{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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
   data Render Ray V3 Double = MRay [SceneItem]
   type Result Ray V3 Double = Scene
   data Options Ray V3 Double = RayOptions
  
   renderRTree _ _ = go where
     go :: RTree Ray V3 Double a -> Scene
     go (Node (RPrim p) _)   = unRay $ render Ray p
     --we ignore textures atm
     go (Node (RStyle s) ts) = concatMap go ts
     go (Node _ ts)          = concatMap go ts

unRay :: Render Ray V3 Double -> Scene
unRay (MRay x) = x


class ToObject t where
      toObject :: t -> SceneItem

instance ToObject (Ellipsoid Double) where
  toObject (Ellipsoid t) = rayPrimSphere t

rayPrimSphere t = SIObject $ Object P.Sphere (rayTrans t)

rayTrans :: T3 Double -> RayModifier
rayTrans t = RM (getTransformation $ inv t)                    

getTransformation = listToMatrix . matrixHomRep 

-- | Convert list to homogenious matrix.
listToMatrix :: (Num a) => [[a]] -> M44 a
listToMatrix (x:y:z:w:_) = transpose $ V4 (homVector x 0) (homVector y 0) 
                                          (homVector z 0) (homVector w 1)

homVector (x:y:z:_) = V4 x y z

instance Renderable (Ellipsoid Double) Ray where
      render _ = wrapSolid . toObject

wrapSolid :: SceneItem -> Render Ray V3 Double
wrapSolid = MRay . (:[]) 

--------------------------------------------------------------------
-- Renderable Camera
--------------------------------------------------------------------

instance Renderable (Camera PerspectiveLens Double) Ray where
  render _ c = MRay [ SICamera $ SCamera {
    cType   = t,
    pos     = l,
    forward = forLen *^ forUnit,
    right   = rightLen *^ rightUnit,
    up      = upUnit
    }]
    where
      l         = camLoc c .-. origin
      (PerspectiveLens h v) = camLens c
      forUnit   = fromDirection . camForward $ c
      forLen    = 0.5*rightLen/tan(h^.rad/2)
      upUnit    = fromDirection . camUp $ c
      rightUnit = fromDirection . camRight $ c
      rightLen  = angleRatio h v
      t         = Perspective

instance Renderable (Camera OrthoLens Double) Ray where
  render _ c = MRay [ SICamera $ SCamera {
    cType   = t, 
    pos     = l,
    forward = forUnit,
    right   = h *^ rightUnit,
    up      = v *^ upUnit
    }]
    where
      l               = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit         = fromDirection . camForward $ c
      upUnit          = fromDirection . camUp $ c
      rightUnit       = fromDirection . camRight $ c
      t               = Orthographic
