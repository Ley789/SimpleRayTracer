import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
--example = cylinder # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutX) (90 @@ deg) 
example = --sphere # sc blue # scale 5 # (transform . aboutZ) (90 @@ deg) # ambient 0.2 # translateZ 10 # translateX 10 # diffuse 0.8 # highlight (Specular 0.5 50) 
  cube # sc red # translateX 10 # ambient 0.3 # diffuse 0.8 # highlight (Specular 0.5 5)
main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (500, 500)
