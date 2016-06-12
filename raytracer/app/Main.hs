import Diagrams.Prelude
import CmdLine
import BackendRayTrace


--cameraLight :: Diagram Ray
cameraLight = mm50Camera # translate (V3 0 0 15) 
              <> pointLight white # translate (V3 5 (-0.5) 15)

example = sphere # sc blue  # ambient 0.2 # diffuse 0.6 # highlight (Specular 0.8 20) # (transform . aboutX) (90 @@ deg) # translateX 2

example2 = cone # sc blue  # ambient 0.1 # diffuse 0.6 # highlight (Specular 1 20) # (transform . aboutX) (90 @@ deg)

example3 = cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

mProp = sc white # ambient 0.2 # diffuse 0.6

sHead = sphere # mProp # scale 0.5 # translateY 1.25
sMid = sphere # mProp
sBody = sphere # mProp # scale 1.5 # translateY (-2)
sEye = sphere # sc black # scale 0.1 # ambient 0.2 # diffuse 0.6 # translateY 1.5 # translateZ 0.25
sNose = cone # sc orange # scale 0.30 # ambient 0.2 # diffuse 0.6 #  translateZ 0.30  # translateY 1.25
example4 = sHead <> sMid <> sBody <> sEye # translateX 0.25 <> sEye # translateX (-0.25) <> sNose

--boundBox = cube # sc red # scaleX 1000 #scaleY 1000 # translateX (-500) # translateY (-500) #  translateZ (-5)  # ambient 0.2 # diffuse 0.6

baseShape = sphere # sc blue # ambient 0.1 # diffuse 0.6

props = zipWith (\ s t -> highlight s . translateX t) specs trans
  where specs = zipWith Specular [0, 0.2 ..] [1, 50 ..]
        trans = [-6, -3 ..]

shapes = map (baseShape #) $ take 5 props

sphereScene = example <> cameraLight
coneScene = example2 <> cameraLight
cubeScene = example3 <> cameraLight
specularScene = mconcat shapes <> cameraLight
snowman = example4 <> cameraLight

scenes =
  [ ("sphere", sphereScene)
  , ("cone", coneScene)
  , ("cube", cubeScene)
  , ("specular", specularScene)
  , ("snow", snowman)
  ]

main =  multiMain scenes

--main = animMain ((translateX <$> ui <*> pure example2) <> pure cameraLight)
