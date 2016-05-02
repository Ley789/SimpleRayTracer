% Backend Ray Tracer for Diagrams
% Alexander Lochmann (Michael Färber)
% 3.5.2016

# Haskell

* Haskell is a functional programming language.
* In functional programming, programs are executed by evaluating expressions.
* Haskell is also pure, because it doesn't allow side effects.
* In Haskell expressions are only evaluated when the result is needed for further
  evaluations, this is called lazy evaluation.

# Domain-Specific Language

* Domain is a field of study that defines a set of common requirements and terminology.
* Domain-Specific Language (DSL) is a tool to simplify expressing and solving problems in a defined domain.

## Embedded DSL
A library implemented in the host language (normally a general-purpose language), providing greater abstractions related to the domain.

## External DSL
An independent language with tools (parser, interpreters, ...)


# Diagrams

## Diagrams

* Diagrams is embedded DSL to describe 2D and 3D scenes.
* This allows to combine the expression power of Haskell with
  readable scene defining DSL Diagrams.

## Example

~~~ haskell
hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

diagram = strokeT (hilbert 6) # lc silver
                              # opacity 0.3
~~~

# Ray tracing

Technique to render a image from a defined scene.

## Idea

* Generate rays, starting from the camera, a ray per pixel.
* Intersect rays with geometry.
* Calculate pixel color with the resulting values (shading).


# My work

* Become acquainted with Haskell, Diagrams and ray tracing.
* Extract scene information from Diagrams.
* Implement ray tracer in Haskell that uses the data from diagrams.
* Support the features from Diagrams (shapes, projections, ...).
