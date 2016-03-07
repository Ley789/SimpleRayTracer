module Shade where

import Linear
-- TODO add color
data Material = Material {
  ambient :: Double
, diffuse :: Double
, specular :: Double
, roughness :: Double
}

--\ Return color from surface material, normal and direction to light source.
-- TODO skalarmultiplication with color and return color
lambertain :: Material -> V3 Double -> V3 Double 
lambertain m l n = diffuse m $ dot (normalize l) (normalize l)
