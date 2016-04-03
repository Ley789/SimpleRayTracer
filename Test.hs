import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
example = sphere # sc blue # scale 4 # ambient 0.1 # diffuse 0.5 # highlight (Specular 0.5 5)
main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (800, 800)
