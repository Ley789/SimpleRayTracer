% Backend Ray Tracer for Diagrams
% Alexander Lochmann (Michael Färber)
% 3.5.2016

# Haskell

* Haskell is a functional programming language.
* In functional programming, programs are executed by evaluating expressions.
* Haskell is also pure, because it doesn't allow side effects.
* In Haskell expressions are only evaluated when the result is needed for further
  evaluations, this is called lazy evaluation.
* TODO: Shorten this.

# Domain-Specific Language

* Domain is a field of study that defines a set of
  common requirements and terminology.
* Domain-Specific Language (DSL) is a tool to simplify expressing and solving
  problems in a defined domain.
* TODO: Example?

## Embedded DSL

* A library implemented in the host language (normally a general-purpose language),
  providing greater abstractions related to the domain.
* TODO: Example. Do students really know Embedded SQL in C/C++??

## External DSL

* An independent language with tools (parser, interpreters, ...)
* Example: CSS


# Diagrams

## Diagrams

* Embedded DSL to describe 2D and 3D scenes
* Allows defining readable scenes using Haskell
* TODO: Explain backend??

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

TODO!


# 3D Diagrams

## Example

Code. Really simple. :)

## Output

Picture.


# Ray tracing

Technique to render a image from a defined scene.

## Idea

* Generate rays, starting from the camera, a ray per pixel.
* Intersect rays with geometry.
* Calculate pixel color with the resulting values (shading).


# POV-Ray

Bild?

Beispiels-Ausgabe.

Diagrams -> POV-Ray-Code -> Bild

Status quo.

Disadvantage: dependency on POV-Ray, no animations


# My work

## Mission

Vielleicht: current state vs. future state?
Implement ray tracer in Haskell that renders Diagrams 3D scenes.

## Steps

* Become acquainted with Haskell, Diagrams and ray tracing.
* Extract scene information from Diagrams.
* Implement functional ray tracer

## Supported features

* Boxes, Cylinders, Cones, ...
* Lights: Diffuse / ...
* Cameras: ...
* Materials: ...
* Transformations: Rotation, translation, ...
* Animations
