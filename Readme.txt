To test the raytrace algorithm perform following steps
1. Load Scene.hs in ghci
2. Create a Scene with the functions 
       emptyScene
       insertPrimitive
       basicCircle
       Material 
       Color
 e.g. let scene = insertPrimitive (Material (Colour 1 1 1), basicCircle) emptyScene
3. Function simpleRayTracer s m n
   where s is the scene
         m is the pixel width
         n is the pixel height
   return a list of pixel colors formated as [[Colour]]
4. to safe the pixel array into a ppm file use:
   save_ppm OUTPUTFILE colorArray
e.g. save_ppm "test.ppm" (simpleRayTracer scene 255 255)
