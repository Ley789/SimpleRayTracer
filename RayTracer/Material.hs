module Material where
import Colour


data Material = Material Colour 
                deriving(Show)

noIntersection =  Material (Colour 0 0 0)

colour r g b = Material (Colour r g b)
getColour (Material c) = c
