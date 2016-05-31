import Diagrams.Prelude
import CmdLine
import BackendRayTrace


--cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 (10)) 
              <> pointLight white # translate (V3 3 (-0.5) 1.5)

example = sphere # sc blue  # ambient 0.2 # diffuse 0.6 # highlight (Specular 0.8 20) # (transform . aboutX) (90 @@ deg) # translateX 2

example2 = cone # sc blue  # ambient 0.1 # diffuse 0.6 # highlight (Specular 1 20) # (transform . aboutX) (90 @@ deg)
--  cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

--boundBox = cube # sc green # scale 1000 # ambient 0.7

--baseShape = sphere # sc blue # ambient 0.1 # diffuse 0.6

--props = zipWith (\ s t -> highlight s . translateX t) specs trans
--  where specs = zipWith Specular [0, 0.2 ..] [1, 50 ..]
--        trans = [-6, -3 ..]

--shapes = map (baseShape #) $ take 5 props

scene = example <> cameraLight
scene2 = example2 <> cameraLight
-- MF: TODO: Compile with `ghc -Wall Test` and fix warnings.
-- Also run `hlint *.hs` and fix warnings.
main =  multiMain [("sc1",scene), ("sc2",scene2)]
--main = animMain ((translateX <$> ui <*> pure example2) <> pure cameraLight)
