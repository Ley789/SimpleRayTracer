import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 (10)) 
              <> pointLight white # translate (V3 15 1 5)

example = cone # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutX) (90 @@ deg)  
--  cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

--boundBox = cube # sc green # scale 1000 # ambient 0.7

--baseShape = sphere # sc blue # ambient 0.1 # diffuse 0.6

--props = zipWith (\ s t -> highlight s . translateX t) specs trans
--  where specs = zipWith Specular [0, 0.2 ..] [1, 50 ..]
--        trans = [-6, -3 ..]

--shapes = map (baseShape #) $ take 5 props

scene = example <> cameraLight

main =  save_ppm "spec.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions scene) (500, 500)
