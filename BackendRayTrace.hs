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
import           SceneTypes
import qualified Primitive                     as P
import           Object                        as O
import qualified Colour                        as C
import           Linear

data Ray = Ray
     deriving (Eq,Ord,Read,Show,Typeable)

type instance V Ray = V3
type instance N Ray = Double

instance Monoid (Render Ray V3 Double) where
   mempty = MRay mempty
   MRay s1 `mappend` MRay s2 = MRay $ mappend s1 s2

instance Backend Ray V3 Double where
   data Render Ray V3 Double = MRay Scene
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

instance ToObject (D.Box Double) where
  toObject (D.Box t) = Object P.Box (trans t) 

instance ToObject (Frustum Double) where
  toObject (Frustum r0 r1 t) = Object (P.Cone r0 r1) (trans t) 

rayPrimSphere t = Object P.Sphere (trans t)

trans :: T3 Double -> OModifier
trans t = OModifier (getTransformation t) mempty

getTransformation t = createMapSet $ (listToMatrix . matrixHomRep) t

-- | Convert list to homogenious matrix.
listToMatrix :: (Num a) => [[a]] -> M44 a
listToMatrix (x:y:z:w:_) = transpose $ V4 (homVector x 0) (homVector y 0) 
                                          (homVector z 0) (homVector w 1)

homVector (x:y:z:_) = V4 x y z

instance Renderable (Ellipsoid Double) Ray where
      render _ = wrapSolid . toObject

instance Renderable (D.Box Double) Ray where
      render _ = wrapSolid . toObject

instance Renderable (Frustum Double) Ray where
      render _ = wrapSolid . toObject

wrapSolid :: Object -> Render Ray V3 Double
wrapSolid x = MRay $ setObject [x] mempty  

--------------------------------------------------------------------
-- Edit Style
--------------------------------------------------------------------

addTexture :: Style V3 Double -> Scene -> Scene
addTexture st s = setObject (map (setTexture st) $ s ^. sObjects) s

convertColor :: Color c => c -> C.Colour
convertColor (colorToSRGBA -> (r,g,b,_)) = C.Colour r g b

setTexture sty = 
  (over (oModifier . texture . pigment) (mkPigment sty)) .
  (setProperty sty)

setProperty sty =
  (over (len . tAmbient)   f1) . 
  (over (len . tDiffuse)   f2) .
  (over (len . tSpecular)  f3) .
  (over (len . tRoughness) f4)
     where len = oModifier . texture . property
           (f1,f2,f3,f4) = mkFinish sty

mkPigment :: Style V3 Double -> C.Colour -> C.Colour
mkPigment sty = combine (fmap convertColor (view _sc sty))

mkFinish sty = ((combine $ sty ^. _ambient),
               (combine $ sty ^. _diffuse),
               (combine $ hl  ^? _Just . specularIntensity),
               (combine $ hl  ^? _Just . specularSize))
  where hl = sty ^. _highlight

combine Nothing a = a
combine (Just b) a = b 
--------------------------------------------------------------------
-- Renderable Light
--------------------------------------------------------------------

--------------------------------------------------------------------
-- Renderable Camera
--------------------------------------------------------------------

--TODO Test 
instance Renderable (Camera PerspectiveLens Double) Ray where
  render _ c = MRay $ setCamera (Last . Just $ SCamera {
    cType   = t,
    pos     = l,
    forward = forLen *^ forUnit,
    right   = rightLen *^ rightUnit,
    up      = upUnit
    }) mempty
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
  render _ c = MRay $ setCamera (Last . Just $ SCamera {
    cType   = t, 
    pos     = l,
    forward = forUnit,
    right   = h *^ rightUnit,
    up      = v *^ upUnit
    }) mempty
    where
      l               = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit         = fromDirection . camForward $ c
      upUnit          = fromDirection . camUp $ c
      rightUnit       = fromDirection . camRight $ c
      t               = Orthographic
