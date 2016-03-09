import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
example = sphere # sc blue # scaleX 5 <> sphere # scaleY 2 # translateX 3 # diffuse 0.5
main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (255, 255)
