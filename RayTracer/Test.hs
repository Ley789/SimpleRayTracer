import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 (5)) 
              <> pointLight white # translate (V3 15 0 5)

example = sphere # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutY) (45 @@ deg)
--  cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

--boundBox = cube # sc green # scale 1000 # ambient 0.7

--baseShape = sphere # sc blue # ambient 0.1 # diffuse 0.6

--props = zipWith (\ s t -> highlight s . translateX t) specs trans
--  where specs = zipWith Specular [0, 0.2 ..] [1, 50 ..]
--        trans = [-6, -3 ..]

--shapes = map (baseShape #) $ take 5 props

scene = example <> cameraLight

-- MF: TODO: Compile with `ghc -Wall Test` and fix warnings.
-- Also run `hlint *.hs` and fix warnings.
main =  save_ppm "primSphere.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions scene) (500, 500)
