import Diagrams.Backend.POVRay
import Diagrams.Prelude


cameraLight :: Diagram POVRay
cameraLight = mm50Camera # translate (V3 0 0 (10)) 
              <> pointLight white # translate (V3 15 1 5)

--example = cylinder # sc blue  # ambient 0.2 # diffuse 0.8 # highlight (Specular 0.5 5) # (transform . aboutX) (90 @@ deg)  
--  cube #  (transform . aboutY) (45 @@ deg) # sc green # ambient 0.2 # diffuse 0.8 # highlight (Specular 1 50) # translateX 1

boundBox :: Diagram POVRay
boundBox = cube # sc green # scale 1000 # ambient 0.7

baseShape :: Diagram POVRay
baseShape = sphere # sc blue # ambient 0.1 
shape :: [Diagram POVRay] -> Diagram POVRay
shape l = foldl1 (<>) l

addProperty lo l lp = zipWith combine lo lp   
  where combine o x = o # l x
transIntervall :: [Double]
transIntervall = [-6,-3..6]
propIntervall :: [Double]
propIntervall = [0, 0.2..1]
highlightIn = [1,50..]

specList = map (\(x,y) -> Specular x y) l
     where l = zipWith (,) propIntervall highlightIn

shapeProp =  addProperty (replicate 5 baseShape) diffuse propIntervall
shapeList = addProperty shapeProp translateX transIntervall

example = boundBox <> shape shapeList <> cameraLight
main = putStrLn $ renderDia POVRay POVRayOptions example
