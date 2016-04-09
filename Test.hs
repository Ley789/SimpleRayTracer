import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM


cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 (10)) 
              <> pointLight white # translate (V3 10 0 (5)) 
shape :: Diagram Ray
--example = cylinder # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutX) (90 @@ deg) 
shape = sphere # sc blue # scale 5 # (transform . aboutZ) (90 @@ deg) # ambient 0.2 # translateZ (-5) # translateX 1 # diffuse 0.8 # highlight (Specular 0.5 50) 
--  cube # sc green # ambient 0.3 # diffuse 0.8 # highlight (Specular 0.5 5)

example = shape <> cameraLight
main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (500, 500)

