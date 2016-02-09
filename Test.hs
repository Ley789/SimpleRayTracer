
import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
example = sphere # scaleX 20 # translateX 5 

main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) 255 255 
