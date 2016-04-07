import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
example = cylinder # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutX) (90 @@ deg) 
--sphere # sc blue # scale 5 # ambient 0.2 # translateZ 10 # diffuse 0.8 # highlight (Specular 0.5 5) <> cube # sc red # rotateAbout (origin) (direction (V3 1 0 0)) (30 @@ deg) # translateX 5 # ambient 0.3 # diffuse 0.8 # highlight (Specular 0.5 5)
main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (500, 500)
