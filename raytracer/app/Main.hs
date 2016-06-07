import Diagrams.Prelude
import CmdLine
import BackendRayTrace


--cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 20) 
              <> pointLight white # translate (V3 3 (-0.5) 1.5)

example = sphere # sc blue  # ambient 0.2 # diffuse 0.6 # highlight (Specular 0.8 20) # (transform . aboutX) (90 @@ deg) # translateX 2

example2 = sphere # sc blue  # ambient 0.1 # diffuse 0.6 # highlight (Specular 1 20) # (transform . aboutX) (90 @@ deg) <> cube # sc red # scaleX 1000 #scaleY 1000 # translateX (-500) # translateY (-500) #  translateZ (-5)  # ambient 0.2 # diffuse 0.6 
--  cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

boundBox = cube # sc red # scaleX 1000 #scaleY 1000 # translateX (-500) # translateY (-500) #  translateZ (-5)  # ambient 0.2 # diffuse 0.6
fsh = sphere # sc blue

shapes = map (\x -> fsh # translateX x) translist

ambientShapes = mconcat $ take 5 $ zipWith (#) shapes $ map (\x -> ambient x) propInter
diffuseShapes = mconcat $ take 5 $ zipWith (#) (map (# ambient 0.2) shapes) $ map (\x -> diffuse x) propInter
spectralShapes = mconcat $ take 5 $ zipWith (#) (map (# diffuse 0.5) ((map (# ambient 0.2) shapes))) $ map (\x -> highlight (Specular x 150)) propInter

propInter = [0, 0.2 ..]
translist = [-6, -3..]

--shapes = map (sphere # sc blue #) $ take 5 (cprops ambient)

scene = example <> cameraLight
scene2 = example2 <> cameraLight
ambientScene = cameraLight <> ambientShapes 
diffuseScene = cameraLight <> diffuseShapes <> boundBox
spectralScene = cameraLight <> spectralShapes <> boundBox

main =  multiMain [("sc1",scene), ("sc2",scene2), ("ambient", ambientScene), ("diffuse",diffuseScene), ("spectral",spectralScene)]
--main = animMain ((translateX <$> ui <*> pure example2) <> pure cameraLight)
