% Backend Ray Tracer for Diagrams
% Alexander Lochmann (Michael Färber)
% 3.5.2016

# Haskell

* Haskell is a purely functional programming language.
* In Haskell expressions are only evaluated when the result is needed for further
  evaluations, this is called lazy evaluation.

# Domain-Specific Language

* Domain is a field of study that defines a set of
  common requirements and terminology.
* Domain-Specific Language (DSL) is a tool to simplify expressing and solving
  problems in a defined domain.

# DSL

## Embedded DSL

* A library implemented in the host language (normally a general-purpose language),
  providing greater abstractions related to the domain.

### Example embedded SQL in Java

~~~ java
String query = "Select * From table";
Statement s = connection.createStatement();
ResultSet queryResult = s.executeQuery(query);
~~~

## External DSL

* An independent language with tools (parser, interpreters, ...)
* Example: CSS

# Diagrams

## Diagrams

* Embedded DSL to describe 2D and 3D scenes
* Allows defining readable scenes using Haskell
* Backend in Diagrams render image form the Diagrams scene

## 2D example

~~~ haskell
hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)
~~~

# Output

\centerline{\includegraphics[scale=0.25]{Hilbert.png}}

# 3D Diagrams

## Example

~~~ haskell
cameraLight = mm50Camera # translate (V3 0 0 (5))
              <> pointLight white
                 # translate (V3 0 5 1)

shape = cube # sc blue  # ambient 0.1 # diffuse 0.8
  # (transform . aboutX) (45 @@ deg)
  # translateX (-0.5)

example = shape <> cameraLight

~~~

# Output

\centerline{\includegraphics[scale=0.25]{Cube.png}}

# Ray tracing

Technique to render a image from a defined scene.

## Idea

* Generate rays, starting from the camera, a ray per pixel.
* Intersect rays with scene objects.
* Calculate pixel color with the resulting values (shading).

# POV-Ray

* POV-Ray is a open source ray tracer which uses text-base scene
  description to render images.

  \centerline{\includegraphics[scale=0.15]{POV.png}}

# Diagrams current state

* Render image form a 3D scene in Diagrams.

## Steps

* Define scene in Diagrams.
* Translate to POV-Ray text-base description
* Execute POV-Ray with translated scene.

##Disadvantage

* Dependency on POV-Ray
* Does not allow animations

# My work

## Mission

Remove the dependency on POV-Ray.
Implement ray tracer in Haskell that renders Diagrams 3D scenes.

## Steps

* Become acquainted with Haskell, Diagrams and ray tracing.
* Extract scene information from Diagrams.
* Implement functional ray tracer

## Supported features

* Shapes: Boxes, Cylinders, Cones, Sphere, ...
* Lights: Diffuse, Spectral, Ambient, ...
* Projections: Perspective, Orthographic, ...
* Materials: Texture, Light properties, ...
* Transformations: Rotation, Translation, Scaling, ...
* Animations

# End

* Thank you for your attention. Are there any questions?
