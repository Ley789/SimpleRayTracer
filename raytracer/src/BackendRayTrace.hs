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
import           Data.Maybe                     (fromMaybe)
import           Data.Typeable
import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude              as D hiding (Last (..), view)
import           SceneTypes
import           Scene
import qualified Primitive                     as P
import           Object                        as O
import qualified Data.Colour.RGBSpace          as C
import           Linear
import           Codec.Picture.Saving

import qualified Data.ByteString.Lazy as Lb

data OutputType = PNG | JPG | TGA

data Ray = Ray
     deriving (Eq,Ord,Read,Show,Typeable)

type B = Ray
type instance V B = V3
type instance N B = Double

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
     go (Node (RStyle s) ts) = mconcat $ map (addTexture s . go) ts
     go (Node _ ts)          = mconcat $ map go ts


unRay :: Render Ray V3 Double -> Scene
unRay (MRay x) = x

class ToObject t where
      toObject :: t -> Object

instance ToObject (Ellipsoid Double) where
  toObject (Ellipsoid t) = rayPrimSphere t

instance ToObject (D.Box Double) where
  toObject (D.Box t) = Object P.Box (trans t) 

instance ToObject (Frustum Double) where
  toObject (Frustum r0 r1 t)
    | r0 == r1  = Object (P.Cylinder r0) (trans t) 
    | otherwise = Object (P.Cone r0 r1) (trans t) 

rayPrimSphere :: T3 Double -> Object
rayPrimSphere t = Object P.Sphere (trans t)

trans :: T3 Double -> OModifier
trans t = OModifier (getTransformation t) mempty

getTransformation :: Transformation V3 Double -> Matrices
getTransformation = matricesOfM44 . listToMatrix . matrixHomRep

-- | Convert list to homogenous matrix.
listToMatrix :: [[Double]] -> M44 Double
listToMatrix (x:y:z:w:_) = transpose $  V4 (homVector x 0) (homVector y 0) 
                              (homVector z 0) (homVector w 1)
listToMatrix _ = error "wrong input"

homVector :: [Double] -> Double -> V4 Double
homVector (x:y:z:_) = V4 x y z
homVector _         = error "needs list with minimum 3 elements"

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

convertColor :: Color c => c -> C.RGB Double
convertColor (colorToSRGBA -> (r,g,b,_)) = RGB r g b

setTexture :: Style V3 Double -> Object -> Object
setTexture sty = 
  over (oModifier . texture . pigment) (mkPigment sty) .
   setProperty sty

setProperty :: Style V3 Double -> Object -> Object
setProperty sty =
  over (len . tAmbient)   f1 . 
  over (len . tDiffuse)   f2 .
  over (len . tSpecular)  f3 .
  over (len . tRoughness) f4
     where len = oModifier . texture . property
           (f1,f2,f3,f4) = mkFinish sty

mkPigment :: Style V3 Double -> C.RGB Double -> C.RGB Double
mkPigment sty = combine (fmap convertColor (view _sc sty))

mkFinish :: Style v n -> (Double -> Double, Double -> Double, Double -> Double, Double -> Double)
mkFinish sty = (combine $ sty ^. _ambient,
                combine $ sty ^. _diffuse,
                combine $ hl  ^? _Just . specularIntensity,
                combine $ hl  ^? _Just . specularSize)
  where hl = sty ^. _highlight

combine :: Maybe c -> c -> c
combine = flip fromMaybe

--------------------------------------------------------------------
-- Renderable Light
--------------------------------------------------------------------
instance Renderable (ParallelLight Double) Ray where
  render _ (ParallelLight v c)
    = MRay $ setLight [Light p c'] mempty
      where p = point $  negated (1000 *^ v)
            c' = convertColor c

instance Renderable (PointLight Double) Ray where
  render _ (PointLight (P p) (convertColor -> c))
    = MRay $ setLight [Light (point p) c] mempty

--------------------------------------------------------------------
-- Renderable Camera
--------------------------------------------------------------------


instance Renderable (Camera PerspectiveLens Double) Ray where
  render _ c = MRay $ setCamera (Last . Just $ SCamera {
    cType   = t,
    pos     = point l,
    forward = vector $ forLen *^ forUnit,
    right   = vector $ rightLen *^ rightUnit,
    up      = vector upUnit
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
    pos     = point l,
    forward = vector forUnit,
    right   = vector $ h *^ rightUnit,
    up      = vector $ v *^ upUnit
    }) mempty
    where
      l               = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit         = fromDirection . camForward $ c
      upUnit          = fromDirection . camUp $ c
      rightUnit       = fromDirection . camRight $ c
      t               = Orthographic



--------------------------------------------------------------------------------

renderScene :: (Monoid m, Semigroup m) => FilePath-> OutputType -> Int -> Int -> QDiagram Ray V3 Double m -> IO ()
renderScene path t h w s =
  case t of
    PNG -> Lb.writeFile path $ imageToPng  f
    JPG -> Lb.writeFile path $ imageToJpg 100 f
    TGA -> Lb.writeFile path $ imageToTga f
  where f = simpleRayTracer (renderDia Ray RayOptions s) (h, w)

