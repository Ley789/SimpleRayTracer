import Diagrams.Prelude
import BackendRayTrace
import qualified Scene as S
import Fast_PPM

example :: Diagram Ray
example = sphere # scale 0.5 # translateZ (-1) <>  
          mm50Camera 

main =  save_ppm "test.ppm" $ S.simpleRayTracer (renderDia Ray RayOptions example) (255, 255)
