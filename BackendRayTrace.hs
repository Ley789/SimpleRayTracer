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
import           Data.List                      (last, foldl1')
import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude              as D hiding (Last (..), view)
import           Scene
import qualified Primitive                     as P
import           Object
import qualified Colour                        as C
import           Linear

data Ray = Ray
     deriving (Eq,Ord,Read,Show,Typeable)

type instance V Ray = V3
type instance N Ray = Double

instance Monoid (Render Ray V3 Double) where
   mempty = MRay (mempty,[])
   MRay (c1, l1) `mappend` MRay (c2, l2) = MRay (mappend c1 c2, l1 ++ l2)

instance Backend Ray V3 Double where
   data Render Ray V3 Double = MRay (Last SCamera, [Object])
   type Result Ray V3 Double = Scene
   data Options Ray V3 Double = RayOptions
  
   renderRTree _ _ = go where
     go :: RTree Ray V3 Double a -> Scene
     go (Node (RPrim p) _)   = unRay $ render Ray p
     go (Node (RStyle s) ts) = foldl1' mappend $ map (addTexture s . go) ts
     go (Node _ ts)          = foldl1' mappend $ map go ts


unRay :: Render Ray V3 Double -> Scene
unRay (MRay x) = x

class ToObject t where
      toObject :: t -> Object

instance ToObject (Ellipsoid Double) where
  toObject (Ellipsoid t) = rayPrimSphere t

rayPrimSphere t = Object P.Sphere (rayTrans t)

rayTrans :: T3 Double -> OModifier
rayTrans t = OModifier (getTransformation t) 
                       (Texture (Last . Just $ C.Colour 1 1 1) mempty)

getTransformation = Last . Just . listToMatrix . matrixHomRep 

-- | Convert list to homogenious matrix.
listToMatrix :: (Num a) => [[a]] -> M44 a
listToMatrix (x:y:z:w:_) = transpose $ V4 (homVector x 0) (homVector y 0) 
                                          (homVector z 0) (homVector w 1)

homVector (x:y:z:_) = V4 x y z

instance Renderable (Ellipsoid Double) Ray where
      render _ = wrapSolid . toObject

wrapSolid :: Object -> Render Ray V3 Double
wrapSolid x = MRay (Last Nothing ,[x])  

--------------------------------------------------------------------
-- Edit Style
--------------------------------------------------------------------

-- ASK better structure for scene, so if the type does change the function does still work
addTexture :: Style V3 Double -> Scene -> Scene
addTexture st (c,l) = (c, map (setTexture st) l)

convertColor :: Color c => c -> C.Colour
convertColor (colorToSRGBA -> (r,g,b,_)) = C.Colour r g b


setTexture :: Style V3 Double -> Object -> Object
setTexture sty (Object o om) = Object o (om `mappend` 
                 OModifier mempty (Texture (mkPigment sty) (mkFinish sty)))


mkPigment :: Style V3 Double -> Last C.Colour
mkPigment = Last . fmap convertColor . view _sc


mkFinish :: Style V3 Double -> TPropertie
mkFinish sty = TPropertie
               (Last $ sty ^. _ambient)
               (Last $ sty ^. _diffuse)
               (Last $ hl  ^? _Just . specularIntensity)
               (Last $ hl  ^? _Just . specularSize)
  where hl = sty ^. _highlight

--------------------------------------------------------------------
-- Renderable Light
--------------------------------------------------------------------

--------------------------------------------------------------------
-- Renderable Camera
--------------------------------------------------------------------

instance Renderable (Camera PerspectiveLens Double) Ray where
  render _ c = MRay (Last . Just $ SCamera {
    cType   = t,
    pos     = l,
    forward = forLen *^ forUnit,
    right   = rightLen *^ rightUnit,
    up      = upUnit
    }, [])
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
  render _ c = MRay (Last . Just $ SCamera {
    cType   = t, 
    pos     = l,
    forward = forUnit,
    right   = h *^ rightUnit,
    up      = v *^ upUnit
    }, [])
    where
      l               = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit         = fromDirection . camForward $ c
      upUnit          = fromDirection . camUp $ c
      rightUnit       = fromDirection . camRight $ c
      t               = Orthographic
