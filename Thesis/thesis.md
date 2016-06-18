**cite more, IEEE double representation, blinn phong, rendering book**

# Introduction

Diagrams is a declarative DSL for creating 2D and 3D scenes in Haskell. It is
optimized for simplicity and flexibility. This is achieved by exploiting monoids.
The standard Backend for 3D scenes is POV-Ray, which is a open source ray tracer.

Ray tracing is a well known rendering technique in the context of computer graphics.
Images can be rendered, with ray tracing, by tracing the path of rays that are emitted
from a camera that travel through pixels of an image plane. Ray tracing is also known
for being able to produce physically realistic images. Ray tracing did not emerge as the
primary rendering method because of its computational intensity. Rasterization based
rendering offered better performance and is now the well established rendering method
in interactive applications. An active field of study is the optimization of the ray tracing
algorithm. Therefore there are a lot of techniques to accelerate ray tracing and improve
the quality of the resulting images. Some of these improvements can be applied to all
ray tracing methods like acceleration data structures.

POV-Ray renders scenes based on a text-base
scene description file. It can render scenes described in Diagrams by translating
the Diagrams DSL to the text-base scene description file of POV-Ray.
The aim of this bachelor project is to remove the dependency on POV-Ray.

In this thesis we provide an alternated Backend for Diagrams.
This is achieved by implementing a native renderer. The renderer is based on
the basic ray tracing algorithm. The thesis provides additionally the integration
into Diagrams.

This work documents the data structure and algorithms mostly mathematically.
It describes the structure of the implementation, e.g. the tasks of the different modules.
It also describes the problems occurred while implementing the ray tracer and
the functional behavior to solve these issues.

In \autoref{rendering} an introduction into the topic rendering is given, focused mainly on
ray tracing. Next \autoref{d-scene} defines the used data structure in the
implementation. In the \autoref{domain-specific-language} we shortly introduced
DSL and Diagrams. Then in the \autoref{algorithms-used-in-the-implementation} we introduce
the algorithm details of the implementation. It contains the intersection function,
normal vector calculations and an introduction to shading.
In \autoref{implementation-and-integration} we describe the structure
of the implementation and the exploited functional behavior.

# Rendering

In the field of 3D computer graphics the process of generating an image from a Euclidean
space with geometric shapes is called rendering. Also the result of this process is a rendering.

We introduce 2 rendering techniques called rasterization and ray tracing.
At the end of the chapter we compare both methods.

## Rasterization

Rasterization is the most used rendering technique for real time applications.
Rasterization executes at least the following steps:

* Calculate the position, color and various attributes of the primitives.
* Convert the primitives into fragments, which are stored in a raster image.
  This raster image stores location, color, depth and other informations.
* Calculate the final color of each pixel based on the raster image.

For a further introduction on this topic see \cite{shre}.

## Ray Tracing

The idea of ray tracing is to trace the path of rays emitted from a camera that travel
through a pixel grid. We can determine the visible object for each ray by intersecting
the ray with each object in the scene. The visible object is the one with the closest intersection.
We can calculate the color with the properties (location, material, etc. ) of the closest object
and the scene. An intersection can generate new rays to simulate effects like reflection and
refractions. There are several ray tracing techniques that share the same basic algorithm.

### History

"Some techniques for shading machine renderings of solids" is a paper by Arthur Appel
published in 1968 \cite{appel}. It is the first approach to ray tracing. He represented rays
as mathematical lines and checked if there is an intersection with an object. The rays
are not traced further after an intersection. That algorithm is known as Ray Casting.

The first ray tracing technique was introduced by Whitted in 1980 \cite{whitt}, which
is an extension of Appels algorithm. After an intersection the ray can generate 3 new
rays, the shadow ray, reflection ray and refraction ray. These rays are called secondary
rays. The starting point of a shadow ray is the intersection point and its direction leads
to a light source. This is used to determine if the light source influences the object at
the intersection point. For example the light source does not influence an object if there
is another object between the generated shadow ray and the light source. Reflection and
refraction rays are traced further, which makes the algorithm recursive.

Distribution ray tracing was introduced by Cook in 1984 \cite{cook}. This method
increases realism of the image by using probability distributions for
specific effects and increases the number of generated rays to approximate the result. For
example generating multiple shadow rays for area light sources to represent soft shadows.

Path tracing was introduced by Kajiya in 1986 \cite{kaji}. Applying distributing
rays not only to specific effects like shadows, but for the shading of all diffuse surfaces.
Thereby indirect illumination of lights with objects can be simulated. This method is also
called Monte Carlo ray tracing, because it uses random samples to compute the image.

### Basic Algorithm

The basic concept of ray tracing algorithms is efficiently finding intersections between
a ray and a scene that consists of a set of geometric primitives \cite{wald}.
The ray, as defined in the \autoref{ray}, can have additional parameters
$t_{min}$ and $t_{max}$, which specify the interval of $t$ used for the points
traversed by the ray. In other words it
specifies the minimum and the maximum distance of a ray.

The algorithm can be split in 3 tasks. The most fundamental task is to find
the closest intersection. The second task, also called visibility/occlusion test, is to check
if there are any intersections. The last task is to find all intersections. Checking for any
intersections is slightly simpler than checking for the closest, so there are algorithms that
are more efficient in this case. For example the occlusion test is used for shadow rays \cite{wald}.

In ray tracing only one condition must apply for a primitive, which is that
there must be a function that can calculate an intersection between the primitive and a
ray. That means primitives can be of various types, from simple geometric shapes like
spheres, cubes, triangles, etc., to complex shapes and parametric patches like the Bézier patches
as long as there exists an intersection function. The flexibility of
primitives allows to represent shapes with full accuracy. Although using multiple kinds
of primitives does not limit the kinds of scenes that can be rendered. Like mentioned
in \autoref{rasterization} most real time applications use rasterization techniques to render an
image and most of them only use triangles as primitives.

Testing every primitive in the scene for an intersection with a ray produces the correct
result, but the computation time extends with each ray and each primitive. For complex
scenes it is necessary to reduce the set of primitives that the ray could intersect. It
is common to use acceleration data structures to preserve consistency, e.g. grids and
kd-trees \cite{copy}.


### Performance

At the beginning of modern computer graphics only scenes with a small number of
primitives were used and interactive graphic applications were not established \cite{copy}. Ray
tracing is computationally intense and aims to simulate physical realistic behavior. These
are part of the reasons why rasterization is the well-established rendering technique for
interactive applications at the moment and ray tracing is still used rarely in this field.
However the demand for more detailed scenes, larger scenes and physical realism leads to
the argument that ray tracing will outperform rasterization at some point because of the
logarithmic scene complexity \cite{wald}.

Ray tracing is usually used for offline rendering due to the fact that it
is computationally intense. The crucial factors for ray tracing algorithms
are:

* The amount of rays. Consider an image with a resolution of 800 $\times$ 800,
  without counting the secondary rays, this lead to a total amount of
  640.000 rays. Depending on the scene complexity the set
  of primary rays may only be small part compared to the set of all rays.

* Scene complexity. Reducing the set of primitives for a ray with acceleration
  data structures is the most important optimization in ray tracing because
  it can give the algorithm a logarithmic behavior in scene complexity \cite{copy}.

The common crucial factors lead to common optimizations. For the amount
of rays, it suffices to reduce the number of rays to reduce the computation time.
This can be achieved by reducing the primary rays (smaller resolution) or reducing the number of
secondary rays. The second fact, the scene complexity, can be optimized by using the
mentioned acceleration data structures. Also using optimized intersection functions leads
to reduced computation time.

Ray tracing is known to be massively parallel problem, because the result of each
ray can be calculated independently. The optimization of ray tracing is a topic of interest
since the invention of the algorithm \cite{copy}.

## Ray Tracing compared with rasterization

The advantages of ray tracing are:

  * Ray tracing is known for physically realistic high quality images.
  * The algorithm is an embarrassingly parallel problem.
  * The algorithm scales logarithmically with the scene complexity.
  * Complex effects like shadows, reflection and refractions can be simulated
    and represented correctly.

Advantages of rasterization:

  * Established method with approximations of physical effects.
  * Part of the rasterization algorithm is already executed by the hardware \cite{shre}.
  * In general smaller computation time.

Rasterization based rendering needs a number of approximations for specific effects,
e.g. shadow maps to approximate shadows. Without specific criteria rasterization may
not be able to simulate or approximate certain effects.

Ray tracing can use approximations to save computation time.
However rasterization needs them for certain effects.

# 3D Scene

In this chapter we introduce definitions that we use to describe 3D scenes.

## Vector notation

We introduce notation conventions which we use in this thesis.
The multiplication of a vector $\vec{v}$ with a scalar $t$ is denoted as
$\vec{p} * t = t * \vec{p}$.

The standard scalar product of vectors $\vec{p}$ and $\vec{q}$ is denoted as
$\langle\vec{p},\vec{q} \rangle $,
where $\langle \vec{q} \rangle = \langle \vec{q},\vec{q} \rangle$.

The Euclidean norm of a vector $\vec{q}$ is denoted as $||\vec{q}||$.

## Homogeneous coordinates

Given a vector $\vec{p}=(x_1,...,x_n)$, the vector
$\vec{q}=(x_1 * w, ..., x_n * w, w)$ with $w \ne 0$ is a homogeneous point
of the vector $\vec{p}$ and the vector $\vec{q'}=(x_1, ..., x_n, 0)$
is a homogeneous direction of the vector $\vec{p}$.

It follows that for every scalar $t \in \mathbb{R}$ and
$t \ne 0$, $t*\vec{q}$ is a homogeneous point and $t*\vec{q'}$ is a
homogeneous direction of $\vec{p}$.

Representing coordinates of $\mathbb{R}^3$ as homogeneous vectors
simplifies transformations because points and directions have different
properties. If not stated otherwise, every point/direction is considered
a homogeneous point/direction except for the definitions in
\autoref{primitives}.


## Color

A color is represented as vector $\vec{c} = (r,g,b)$, with values in
range of [0,1]. The components are called channels, where each channel
represents light intensity with $r$ as the red, $g$ as the green and
$b$ as the blue channel. We use the RGB color model to calculate the resulting
color.

E.g. $\vec{c} =(1, 0, 0)$ has a red channel of 100%, green channel of
0% and blue channel 0%, so the resulting color is red with full intensity.


## Camera

A camera is defined as 4-tuple $(\vec{p}, \vec{f} , \vec{r}, \vec{u})$
of vectors, where $\vec{p}$ is the position,
$\vec{f}$ is the forward direction, $\vec{r}$ is the right direction and
$\vec{u}$ is the up direction.

The relation between the vectors is explained in \autoref{projection}.

## Viewing plane

The viewing plane matrix with $n$ rows and $m$ columns is defined as

$$
  V_{ij} = (-f(i,n), f(j,m))  
$$

where $1 \le i \le n$, $1 \le j \le m$ and $f(x,y)= ((x - 1)/ (y - 1)) - 0.5$.
The rows and columns of the viewing plane determine the resolution.

The viewing plane represents the image grid, e.g. the pixels that will be rendered.

## Light

A light is defined by a tuple $(\vec{p}, \vec{c})$, where $\vec{p}$ is the position and
$\vec{c}$ the color of the light.

We assume that the light illuminates objects with the specified color uniformly
in all directions. We also assume that illumination intensity does not fall off with
distance.


## Ray

A ray is defined as a tuple $(\vec{o}, \vec{d})$, where $\vec{o}$ is called
the position and $\vec{d}$ the direction of the ray.
The ray travels through all points of the set

$$
 \{ \vec{o} + t * \vec{d} | t \in \mathbb{R}, t > 0 \}.
$$


## Primitives

We define a number of shapes, called primitives, that can be represented in
the Euclidean space. Every shape is defined by the set of the points of its
surface.

### Sphere

The unit sphere, which is centered at the origin, is defined by the set

$$
  \{\vec{p} | \vec{p} \in \mathbb{R}^3, ||\vec{p}|| = 1 \}.
$$

A render of a sphere is illustrated in
\autoref{fig:sphere}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.25]{primSphere.png}
\caption{Sphere}
\label{fig:sphere}
\end{figure}

### Box

The box aligned with the axes and all four sides of length 1 is defined by the set

$$
  P_x(0) \cup P_x(1) \cup P_y(0) \cup P_y(1) \cup P_z(0) \cup P_z(1),
$$

where

$$
  P_x(v) := \{(x,y,v) | 0 \le x,y \le 1, x,y \in \mathbb{R}\}
$$
$$
  P_y(v) := \{(v,y,z) | 0 \le y,z \le 1, y,z \in \mathbb{R}\}
$$
$$
  P_z(v) := \{(x,v,z) | 0 \le x,z \le 1, x,z \in \mathbb{R}\}.
$$

A render of a box is illustrated in
\autoref{fig:box}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.25]{primBox.png}
\caption{Box}
\label{fig:box}
\end{figure}

### Cylinder

A cylinder aligned at the z-axis, with length 1 and radius $r \in \mathbb{R}$
is defined by the set

$$
  \{\vec{p}=(x,y,z)| x,y,z \in \mathbb{R}^3, x^2 + y^2 = r,1 \ge z \ge 0 \}.
$$

A render of a cylinder is illustrated in
\autoref{fig:cylinder}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.25]{primCylinder.png}
\caption{Cylinder}
\label{fig:cylinder}
\end{figure}

### Cone

A cone of length 1 which is aligned at the z-axis is defined by the set

$$
  \{\vec{p}=(x,y,z)| x,y,z \in \mathbb{R}^3,
  \cos^2 \alpha * (x^2 + y^2) - \sin^2 \alpha * (z - r_1 / \alpha) = 0,
  1 \ge z \ge 0\},
$$

where the base cap is centered at the origin $\vec{p_1}$ with radius
$r_1 \in \mathbb{R}$, the top cap is centered at $\vec{p_2} = (0,0,1)$ with
radius $r_2 \in \mathbb{R}$ and the tangent of the half-angle is
$\tan \alpha = r_1 - r_2 / ||\vec{p_2} - \vec{p_1}|| = r_1 - r_2$.

This definition allows have different half-angles and apex positions,
which are calculated with the radii.

A render of a cone is illustrated in
\autoref{fig:cone}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.25]{primCone.png}
\caption{Cone}
\label{fig:cone}
\end{figure}

## Transformations

A transformation is a function that takes a vector $\vec{p} = (x_1,...,x_n)$
and returns a vector $\vec{q} = (y_1,...,y_n)$. In the next sections we
introduce a group of transformations.


### Scaling matrix

We define the scaling function as

$$
  S(s_x,s_y,s_z):= \left(
          \begin{array}{cccc}
              s_x & 0   & 0   & 0 \\
              0   & s_y & 0   & 0 \\
              0   & 0   & s_z & 0 \\
              0   & 0   &  0  & 1
           \end{array}
       \right),
$$

where the indices stands for the scaling in the corresponding axes.
A scaling matrix is a result of the scaling function defined above or the
multiplications of 2 scaling matrices.
If $s_x = s_y = s_z$ then the scaling is called uniform.
Multiplying every point of a primitive with a scaling matrix resizes
the primitive.

For example, given $\vec{p} = (1, 1, 1, 1)$ and a scaling matrix
$S(5, 1, 0) = S'$, then $S'\vec{p} = (5, 1, 0, 1)$.

### Rotation matrix

To rotate a primitive in 3D space we need to define by which axis
and which angle $\alpha$ to rotate.

We define the rotation function as

$$
  R_x(\alpha) := \left(
          \begin{array}{cccc}
              1   & 0          & 0           & 0 \\
              0   & \cos \alpha & -\sin \alpha & 0 \\
              0   & \sin \alpha & \cos \alpha  & 0 \\
              0   & 0          & 0           & 1
           \end{array}
       \right)
$$

$$
  R_y(\alpha) := \left(
          \begin{array}{cccc}
              \cos \alpha  & 0 & \sin \alpha & 0\\
              0           & 1 & 0          & 0 \\
              -\sin \alpha & 0 & \cos \alpha & 0 \\
              0           & 0 & 0          & 1
           \end{array}
       \right)
$$

$$
  R_z(\alpha) := \left(
                  \begin{array}{cccc}
                      \cos \alpha & -\sin \alpha & 0 & 0 \\
                      \sin \alpha & \cos \alpha  & 0 & 0 \\
                      0          & 0           & 1 & 0 \\
                      0          & 0           & 0 & 1
                   \end{array}
                  \right),
$$

where the indices indicate the axis by which we rotate and $\alpha$ the angle. A rotation
matrix is a result of the rotation function defined above or the multiplications of 2 rotation
matrices. For a corresponding proof see \cite{kenn}.

Multiplying every point of a primitive with a rotation matrix rotates
the primitive.

To rotate a point $\vec{p}$ on an arbitrary axis $\vec{a}$ by the angle $\alpha$,
we first find a rotation matrix $R_a$ which aligns the axis $\vec{a}$ with one
of the defined axes (x-,y- or z-axis). Next we apply the rotation
$R_s(\alpha)R_a\vec{p} = \vec{p'}$, where $R_s$ is the defined rotation matrix
of the aligned axis. Afterwards we undo the rotation done to align the axis
with $R_a^{-1}$. This leads to the rotated point $\vec{p''} = R_a^{-1}\vec{p'}$.
We can combine these steps to one rotation matrix

$$
  R = R_a^{-1}R_s(\alpha)R_a.
$$

### Translation matrix

A translation function is defined as

$$
  T(x,y,z):= \left(
                \begin{array}{cccc}
                  1 & 0 & 0 & t_x \\
                  0 & 1 & 0 & t_y \\
                  0 & 0 & 1 & t_z \\
                  0 & 0 & 0 & 1
                \end{array}
              \right),
$$

where the indices describe the translation in the corresponding axes. A translation matrix
is a result of the translation function defined above or the multiplications of 2 translation
matrices.

### Transformation matrix

We define a transformation matrix $M$ as multiplications of a
translation $T'$, rotation $R'$ and scaling matrix $S'$

$$
  M := T'R'S'.
$$

Every combination of translating, rotating and
scaling matrices can be represented as transformation matrix.



\begin{proof}

Given a matrix $L$ which is a result of multiplications
of translating, rotation and scaling matrices, we extract
3 matrices $T'$, $R'$ and $S'$ so that $T'R'S' = L$.

The translation can be directly read from the last column
of the matrix $L$.

$$
  T'= \left(
                \begin{array}{cccc}
                  1 & 0 & 0 & L_{1,4} \\
                  0 & 1 & 0 & L_{2,4} \\
                  0 & 0 & 1 & L_{3,4} \\
                  0 & 0 & 0 & 1
                \end{array}
              \right)
$$

To extract the rotation matrix we first need to state an important property
of these matrices.

For all rotation matrices follows that the scalar product
of each row and each column is $1$. This results from the definition in
section \autoref{rotation-matrix}.

We demonstrate with the matrix $L'$ how we extract the scaling components.
For simplification we drop the last row and last column of rotation and
scaling matrices.

$$
  L' = \left(
                \begin{array}{cccc}
                  r_{11} & r_{12} & r_{13} \\
                  r_{21} & r_{22} & r_{23} \\
                  r_{31} & r_{32} & r_{33}
                \end{array}
        \right)
        *
        \left(
                \begin{array}{cccc}
                s_x & 0   & 0 \\
                0   & s_y & 0 \\
                0   & 0   & s_z \\
                \end{array}
        \right)
        =
        \left(
                \begin{array}{cccc}
                    r_{11} * s_x & r_{12} * s_y & r_{13} * s_z \\
                    r_{21} * s_x & r_{22} * s_y & r_{23} * s_z \\
                    r_{31} * s_x & r_{32} * s_y & r_{33} * s_z
                \end{array}
        \right)
$$

$$
\begin{aligned}
  s_x &= \sqrt{(r_{11} * s_x)^2 + (r_{21} * s_x)^2 + (r_{31} * s_x)^2} \\
      &= \sqrt{ r_{11}^2 * s_x^2 + r_{21}^2 * s_x^2 + r_{31}^2 * s_x^2} \\
      &= \sqrt{ s_x^2 * (r_{11}^2 + r_{21}^2 + r_{31}^2)} \\
      &= \sqrt{ s_x^2} * \sqrt{r_{11}^2 + r_{21}^2 + r_{31}^2} \\
      &= \sqrt{ s_x^2} = s_x
\end{aligned}
$$

We know that $\sqrt{r_{11}^2 + r_{21}^2 + r_{31}^2} = 1$ because of the property
of rotation matrices. The other components of the scaling matrix can be extracted
analogously. Now we can create the scaling matrix $S'$.

We also see that we can extract the scaling components for the matrix $S'$
form arbitrary ordered multiplications of scaling and rotation matrices.

The remaining matrix $R'$ can be obtained by multiplying the inverse scaling
matrix with $L''$, where $L''$ is the matrix $L$ but with the last column
replaced by $\vec{v}=(0, 0, 0, 1)^T$.

$$
  R' = L'' * S'^{-1}
$$
\end{proof}

## Material property

A material property is a vector $\vec{p} = (a, d, s, r)$ whose values are in
the interval $[0,1]$. The components are called coefficients with $a$ as the
ambient, $d$ as the diffuse, $s$ as the specular and $r$ as the rough
coefficient.

These values are used in \autoref{shading-model} to manipulate illumination influence. That means
we can adjust the final color by changing these coefficients.

## Object

An object is a 4-tuple $(pr, M ,\vec{c}, \vec{p})$, where $pr$ is a primitive,
$M$ is a transformation matrix, $\vec{c}$ is a color and $\vec{p}$ is a
material property.


# Domain-Specific Language

A Domain-Specific Language (DSL) is a language that simplifies expressions and
solves problems in a defined domain. There are 2 major kinds of DSL

* Embedded DSL: An embedded DSL is a library implemented in the host language
   (normally a general-purpose language), providing greater abstractions related to
    the domain. An example is SQL database connector for Java.
* Internal DSL: An internal DSL is an independent language with several tools (parser,
    interpreters, . . . ). A common used internal DSL is CSS.

In this chapter we introduce Diagrams. It is a embedded DSL for vector graphics and
3D scenes. Next we discuss the usability of Diagrams in 3D.

## Diagrams

Diagrams is an embedded DSL with host language Haskell that provides powerful and
flexible ways to create vector graphics. Diagrams is optimized for simplicity and flexibility
rather than for speed. It uses the abstractions of Haskell to provide these features.
Diagrams allows to describe 2D and 3D scenes. An example of a Haskell 2D scene

\begin{minted}{haskell}
hilbert 0 = mempty
  hilbert n = hilbert' (n-1)         # reflectY <> vrule 1
             <> hilbert (n-1)        <> hrule 1
             <> hilbert (n-1)        <> vrule (-1)
             <> hilbert' (n-1)       # reflectX
    where
       hilbert' m = hilbert m        # rotateBy (1/4)
\end{minted}

In this Haskell code a function hilbert is defined recursively. The function
uses monads to concatenate scene elements (<> is mappend in infix notation). The
scene elements (vrule and hrule), the properties (reflectX, reflectY and rotateBy)
and the function # which combines them are part of the Diagrams DSL.

A illustration of that scene is in \autoref{fig:hilbert}

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.25]{Hilbert.png}
\caption{Hilbert}
\label{fig:hilbert}
\end{figure}

### Backend

Diagrams offers an interface to render the described scenes, which is called backend. The
only existing backend for Diagrams 3D translates the scene to the text-base description
language of POV-Ray, an open source renderer.

### Advantage of Diagrams over POV-ray

There are several advantages of using Diagrams over POV-Ray

* Diagrams can describe more scenes than the text-base description of POV-Ray. For
     example an animation in Diagrams is a scene that changes over time. This cannot
     be described in POV-Ray.
* Scene descriptions are generally shorter in Diagrams than in POV-Ray because
     Diagrams scenes can take advantages of the expression power of Haskell.
* Extension of Diagrams is independent of the backend. That means that backends
  can support the features of Diagrams. A extension of the text-base scene
  description of POV-Ray only makes sense if POV-Ray supports the extension.

# Algorithms used in the implementation

In this chapter we introduce elements that are needed to implement the basic ray tracing
algorithm. First we discuss the intersection functions for the defined primitives in \autoref{primitives}.
After that we show a method to keep the intersection functions simple even with
transformations. We describe how transformations influence normal vectors. Next
we describe the generation of primary rays and how they lead to different projections. At
the end we introduce a well known shading algorithm to approximate light behavior.


## Intersection functions

The intersection functions are based on Euclidean space. So we first need to
convert a ray to Euclidean coordinates. Given a ray $R$ with origin at point
$\vec{o} = (o_x,o_y,o_z,w)$ and direction $\vec{d} = (d_x, d_y, d_z, 0)$, then
the ray with the origin $\vec{o'}= (o_x / w, o_y / w, o_z / w)$ and the direction
$\vec{d'}= (d_x, d_y, d_z)$ represents $R$ in Euclidean space.

In the following subsections we denote $\vec{o}=(x_o, y_o, z_o)$ as origin and
$\vec{d} = (x_d, y_d, z_d)$ as direction of a ray and for all points and
directions follows that they are denoted in Euclidean space.

### Sphere intersection

We see from the definition of the sphere \autoref{sphere} that we have to solve
the equation

$$
  \langle \vec{o} + t*\vec{d}, \vec{o} + t*\vec{d} \rangle  = 1
$$

This can be simplified to the quadratic equation

$$
  \langle \vec{o},\vec{o} \rangle  + t * 2 * \langle \vec{o}, \vec{d} \rangle  + t^2 *
  \langle \vec{d}, \vec{d} \rangle  - 1 = 0
$$

Now we can solve the quadratic equation

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a},
$$

where $a = \langle \vec{d},\vec{d} \rangle $, $b=2* \langle \vec{o},\vec{d}
\rangle $ and $c= \langle \vec{o},\vec{o} \rangle $.
The smallest positive result of the quadratic equation leads to the nearest
intersection.

### Box intersection

To get the nearest intersection between a ray and a box we check the boundaries.
We show this for the x-component, for the other components it works analogously.

$$
  \begin{split}
  tmin_x = (0 - x_o) * (1 / d_x ) \\
  tmax_x = (1 - x_o) * (1 / d_x )
  \end{split}
$$

Now we have the minimum for the x-component. After calculating the minimum and
the maximum values for the other components we take the maximum of all minimums,
$tmin = max (tmin_x, tmin_y, tmin_z)$, and the minimum of all maximums,
$tmax = max (tmax_x, tmax_y, tmax_z)$. If $tmin > tmax$ or $tmin \le tmax <0$ then
there is no intersection. If $tmin < 0$ and $tmax > 0$ then $tmax$ leads to the
nearest intersection. For further details see \cite{will}.

### Plane intersection

A plane is defined by a point $\vec{p}$ that lies on the plane and a normal vector
$\vec{n}$ of the plane. To get the intersection between a ray and a plane we need
to solve the equation

$$
  \vec{n} * (\vec{o} + t * \vec{d} - \vec{p}) = 0
$$

We need to check if the displacement between the ray and $\vec{p}$ is in the
same plane. This leads to the equation

$$
  t = \frac{\langle \vec{n}, \vec{p} - \vec{o} \rangle }{\langle \vec{n}, \vec{d} \rangle }  
$$

### Cylinder intersection

A cylinder aligned on an arbitrary line $\vec{p_a} + \vec{v_a} * t$, a
point on the cylinder $\vec{q}$ and radius $r$ holds
$\langle \vec{q} - \vec{p_a} - \langle \vec{v_a},\vec{q} - \vec{p_a}
\rangle * \vec{v_a} \rangle - r^2 = 0$.


The definition of cylinder in \autoref{cylinder} allows us to simplify
the equation. We substitute the point on the cylinder $\vec{q}$ with the ray

$$
  \langle \vec{o} + t * \vec{d} - (0,0, z_o + t * z_d) \rangle  - r^2 = 0
$$

This can be simplified to

$$
  (x_o + t * x_d)^2 + (y_o + t * y_d)^2 - r^2 = 0
$$

which leads to the following quadratic equation

$$
  t^2 * (y_d^2 + x_d^2) + t * 2 * (x_o * x_d + y_o * y_d) + x_o^2 + y_o^2 - r^2 = 0
$$

Now we can solve the quadratic equation

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a},
$$

where $a = y_d^2 + x_d^2$, $b = 2 * (x_o * x_d + y_o * y_d)$ and
$c = x_o^2 + y_o^2 - r^2$.

To get the nearest intersection of a cylinder perform following steps:

* Solve the quadratic equation.
* Intersect with the planes $p'$, which includes
  the base cap, and $p''$, which includes the top cap.
* Check that $1 \ge z \ge 0$ holds for the point $\vec{q} = (x,y,z)$ which is the
  result of using the smallest positive $t$.
* Check that the plane intersections are in the range of the caps by
  verifying $||\vec{q_1}|| \le r^2$ and $||\vec{q_2}|| \le r^2$, where $\vec{q_1}$
  is the intersection of the ray with plane $p'$ and $\vec{q_2}$ the intersection
  with the plane $p''$.
* Take the nearest intersection.

### Cone intersection

For a cone aligned on an arbitrary line $\vec{p_a} + \vec{v_a} * t$ with apex
$\vec{p_a}$, center of the base cap at $\vec{p_1}$, center of the top cap at
$\vec{p_2}$, a point on the cone $\vec{q}$ and half-angle $\alpha$ holds
$\cos^2 \alpha \langle \vec{q} - \vec{p_a} - \langle \vec{v_a}, \langle \vec{q}
- \vec{p_a} \rangle  \vec{v_a} \rangle - \sin^2 \alpha
\langle \vec{v_a},\vec{q} - \vec{p_a} \rangle  = 0$,

where $\vec{p_a} = \vec{p_1} + r_1 * (\vec{p_2} - \vec{p_1})/(r_1 - r_2)$.

Same as for the cylinder, substitute the ray with $\vec{q}$ and solve the
equation for $t$. The definition of cone in \autoref{cone} allows us
to simplify the resulting components for the quadratic equation

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a}
$$

with

$$
  \begin{split}
    a &= \cos^2 \alpha * (x_d^2 + y_d^2) - \sin^2 \alpha * z_d^2 \\
    b &= 2* \cos^2 \alpha * (x_d * x_o + y_d * y_o) - 2 * sin^2 \alpha * z_d * (z_o - \frac{r_1}{r_1 - r_2}) \\          
    c &= cos^2 \alpha * (x_o^2 + y_o^2) - sin^2 \alpha * (z_o - \frac{r_1}{r_1 - r_2})^2,
  \end{split}
$$

To get the nearest intersection of a cone preform following steps:

* Solve the quadratic equation.
* Intersect with the planes $p'$, which includes
  the base cap, and $p''$, which includes the top cap.
* Check that $1 \ge z \ge 0$ holds for the point $\vec{q} = (x,y,z)$ which is the
  result of using the smallest positive $t$.
* Check that the plane intersections are in the range of the caps by
  verifying $||\vec{q_1}|| \le r_1^2$ and $||\vec{q_2}|| \le r_2^2$, where $\vec{q_1}$
  is the intersection of the ray with plane $p'$ and $\vec{q_2}$ the intersection
  with the plane $p''$.
* Take the nearest intersection.


## Ray transformations

To keep our intersection functions simple, we transform the ray instead
of the primitives. Given a ray $r = \vec{o} + t * \vec{d}$, a transformation
matrix $M = TRS$ and a point $\vec{p}$, we see that

$$
  r = M\vec{p}.
$$

We can multiply both sides with $M^{-1}$ that leads to

$$
  M^{-1}r = M^{-1}M\vec{p} = I\vec{p} = \vec{p}.
$$

Now we simplify the expression because a translation does not affect
a direction.

$$
  M^{-1}(\vec(o) + t * \vec{d}) = M^{-1}\vec{o} + S^{-1}R^{-1}\vec{d} * t
$$

Note that $T^{-1}\vec{d} =\vec{d}$ holds for all directions $\vec{d}$.
We see that we get the same result as applying the transformation on a point.

## Normal vectors

Normal vectors are unit vectors that are orthogonal to a given surface at a given
point. In this subsection we show how to calculate normal vectors for the
primitives.
The normal vectors are used to simulate light effects see \autoref{shading-model}.

### Sphere normal

From the sphere definition in \autoref{sphere} follows that for all
points $\vec{p}$ of the sphere holds that $p$ is the normal vector of the
surface at position $\vec{p}$.

### Plane normal

For each point on the surface holds that they have the same normal vector, which
is already defined in the plane definition in \autoref{plane-intersection}.
With the plane normal we can calculate the normal of the primitive box.

### Cylinder normal

Given a point $\vec{p}=(x,y,z)$ on the surface of a cylinder then the normal
vector is $\vec{n} = (x /m, y /m, 0)$, where $m = \sqrt{x^2 + y^2}$.


### Cone normal

Given a point $\vec{p}=(x,y,z)$ on the surface of a cone, the radius $r_1$ of the
base cap and radius $r_2$ of the top cap, then the apex is
$\vec{p_a} = (0 ,0 , r_1 / (r_1 - r_2))$.

We first calculate the direction
of the normal on the $x/y$ plane which is the vector $\vec{n_{xy}}=(x,y,0)$. After
we calculate the normal direction on the $z$ plane. From trigonometry follows
that the height of the cone is $||\vec{p_a}|| = h$ and the hypotenuse
$c = \sqrt{r_1^2 + h^2}$. Now we can calculate the Euclidean norm for the $z$
and $x/y$ plane component of the normal vector, which is $r_1/c$ for the $z$
plane and $h/c$ for the $x/y$ plane. Now we combine these results

$$
  \vec{n} = ( \frac{x}{x^2 + y^2} * \frac{h}{c},
              \frac{y}{x^2 + y^2} * \frac{h}{c}, \frac{r_1}{c}),
$$

where $\vec{n}$ is the normal vector of the cone at point $\vec{p}$.

### Normal vector transformation


After we apply a transformation $M$ to a point $\vec{p}$ and to its corresponding normal vector $\vec{n}$
it is not guaranteed that $\langle M \vec{n}, M \vec{p} \rangle  = 0$.
For example if we apply a non uniform scaling.
So we need to find a transformation that transforms our normal vector correctly. First
we transform the normal vector to a homogeneous direction. After we see that

$$
  \langle \vec{n}, \vec{p} \rangle  = \vec{n}^{\tr}\vec{p} = 0
$$

which leads to

$$
  \vec{n}^{\tr} I \vec{p} = \vec{n}^{\tr} M^{-1} M\vec{p} = 0,
$$

where $M\vec{p}$ is our transformed point. So the normal vector of the
transformed point is

$$
  \vec{n'}^{\tr} = \vec{n}^{\tr} M^{-1}
$$

this is equivalent to

$$
  \vec{n'} = (\vec{n}^{\tr} M^{-1})^{\tr} = (M^{-1})^{\tr} \vec{n}
$$

with $M = TRS$ follows

$$
  \begin{split}
  (M^{-1})^{\tr} \vec{n} &= (S^{-1}R^{-1}T^{-1})^T \vec{n} \\
                   &= (T^{-1})^{\tr} (R^{-1})^{\tr} (S^{-1})^{\tr} \vec{n} \\
                   &= (T^{-1})^{\tr} R S^{-1}\vec{n}.
  \end{split}
$$

We used the property of rotation matrices that for every rotation matrix
$R$ follows that $(R^{-1})^{\tr} = R$.

## Projection

In the context of computer graphics, a projection transforms points in 3D
onto a 2D grid \cite{kevi}. In ray tracing we change the kind of projection
with the way we generate the rays.

### Perspective

In a perspective projection each line is centered at the camera position \cite{kevi}.
Given a camera $c=(\vec{p},\vec{f},\vec{u},\vec{r})$ and a viewing plane $V$,
we define the matrix of rays that represents a perspective projection as

$$
    P_{ij} :=  (\vec{p}, \vec{f} + snd(V_{ij}) * \vec{r} + fst(V_{ij}) * \vec{u})
$$

where $snd((x,y)) = y$ and $fst((x,y)) = x$. The indices of the matrix
determine the position of the pixels in the resulting image.

### Orthographic

In an orthographic projection every ray has the same direction and has its
center in the view plane. Given a camera $c=(\vec{p},\vec{f},\vec{u},\vec{r})$ and a viewing plane $V$,
we define the matrix of rays that represents an orthographic projection as

$$
    O_{ij} :=  (\vec{f} + snd(V_{ij}) * \vec{r} + fst(V_{ij}), \vec{f})
$$

where $snd((x,y)) = y$ and $fst((x,y)) = x$. The indices of the matrix
determine the position of the pixels in the resulting image.

## Shading model

Shading defines the calculation of the output color for each pixel.
We use a shading model that is described in the book
\cite{kevi}, which is also known as Blinn-Phong shading model.

After generating the projection we intersect each ray with each objects
primitive and we consider only the closest intersection. If there is no
intersection, then the resulting color is black. If we find
an intersection, we use the following defined shading method.

### Ambient light

Simulating indirect illumination is very computationally intense, a common
practice is to assume that illumination is constant throughout the scene.
This is called ambient illumination and it is not a good approximation, but it
provides some illumination for the parts that do not receive direct illumination
\cite{kevi}.

We calculate the ambient color of an object with the ambient coefficient $a$ of
the objects material property multiplied by the color $\vec{c_a} = (1, 1, 1)$.

A illustration of a scene with 5 spheres and changing ambient coefficient is in
\autoref{fig:ambient}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.5]{ambient.png}
\caption{The ambient coefficient increases for each sphere from left
	to right by 0.2 starting from 0.}
\label{fig:ambient}
\end{figure}

### Diffuse light

To simulate direct illumination we first need to check if there is no object
between the intersection point and the light.

We do this by generating a ray $S$
with position $\vec{p}$ and direction $\vec{d} = \vec{l_p} - \vec{p}$,
where $\vec{p}$ is the intersection point and $\vec{l_p}$ is the position of the light.
Then we try to intersect $S$ with every object. If there is no intersection
or the nearest intersection is further away than $\vec{l_p}$, then the light
illuminates the object and we calculate the diffuse color with following
equation

$$
  \vec{c_d} = \vec{c} * d * \max (0, \langle \vec{l_d}, \vec{n} \rangle ),
$$


where $n$ is the normal vector of the surface at position $\vec{p}$, $d$ is the
diffuse coefficient of the objects material property, $\vec{c}$ the color of
the light and $\vec{l_d}$ is the normalized vector $\vec{d}$.

A illustration of a scene with 5 spheres and changing diffuse coefficient is in
\autoref{fig:diffuse}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.5]{diffuse.png}
\caption{The diffuse coefficient increases for each sphere from left
	to right by 0.2 starting from 0.}
\label{fig:diffuse}
\end{figure}


### Specular reflection

To simulate smooth, shiny objects we allow them to reflect light that's
concentrated around the direction of mirror-reflection \cite{kevi}.

For specular reflection we also need to check if there is no object between
the intersection point and the light. We assume that there in between, then
we calculate the specular reflection color with following equation

$$
  \vec{c_d} = \vec{c} * s * \max (0,\langle \frac{\vec{v} + \vec{l_d}}{||\vec{v} + \vec{l_d}||},
     \vec{n} \rangle )^r,
$$

where $n$ is the normal vector of the surface at position $\vec{p}$, $s$ is the
diffuse coefficient, $r$ is the rough coefficient of the objects
material property, $\vec{c}$ the color of the light,
$\vec{l_d}$ is the normalized vector $\vec{d} = \vec{l_p} - \vec{p}$ and $\vec{v}$
is the negated normalized direction of the ray that intersected the object.

A illustration of a scene with 5 spheres and changing specular coefficient is given in
\autoref{fig:specular}.

\begin{figure}[!ht]
\centering
\includegraphics[scale=0.5]{specular.png}
\caption{The specular coefficient increases for each sphere from left
	to right by 0.2 starting from 0. The rough coefficient is 150.}
\label{fig:specular}
\end{figure}


### Multiple lights and final results

The final color $c_f$ is computed

$$
  c_f = c_o * ( c_a + c_d + c_s ),
$$

where $c_a$ is the ambient color, $c_d$ is the diffuse color,
$c_s$ is the specular reflection color and $c_o$ is the color of the object.

If we have multiple lights $I$ then the final color is computed as

$$
 c_f =  c_o * ( c_a + \sum_{i=0}^I c_{di} + c_{si} ),
$$

where $c_{\_i}$ is the color of the i-th light.


# Implementation and Integration

To integrate the ray tracer into Diagrams a backend is needed. This backend extracts
scene informations from a simplified data structure provided by Diagrams. After the
extraction the scene can be rendered. This chapter discusses the implemented modules
and their functionality. At the end we briefly introduce the methods used by the backend.

## Modules

### Primitive

The module primitive defines the primitives and the ray as described in \autoref{primitives}
and \autoref{ray}. It contains the intersection functions described in \autoref{intersection-functions}
and normal vector calculation functions described in \autoref{normal-vectors}.

While intersecting a primitive with a ray we know which normal vector function
is needed to calculate the actual normal vector. This does not generally apply
if we have a intersection point and a primitive because of the numerical error
in the IEEE-754 double representation. For example if we have a intersection point
of a cone that is not represented mathematically correctly then we do not know
if the point lies on the bot cap, top cap or at the cone without allowing some
error. We solved this problem by exploiting the laziness property of Haskell.
We calculate the normal vector in the intersection function.
We know that the calculation is only evaluated when the value is needed
because of laziness.

### Object

The module Object defines the scene objects as described in \autoref{object}.
The object stores the needed matrices to save computation time. It preforms
the transformations of the rays and the intersection point as described in
\autoref{ray-transformations}. It uses the monoid instance to simplify the
modifications of the properties of an object.

### SceneTypes

The module SceneTypes defines light, camera and a scene as described in \autoref{d-scene}.
A scene is a instance of the monoid class to combine multiple scenes. The scenes
are combined by inserting the objects and the lights
of both scenes in the new scene and using the last defined camera, which means
the second argument to mappend.

### Blinn-Phong

The module Blinn-Phong calculates the ambient, diffuse and specular reflection color
as described in \autoref{shading-model}. The color operations are executed by
using the applicative class.

### Scene

The module Scene generates primary rays to represent the projection,
depending on the camera type, as described in \autoref{projection}. It calculates
the nearest intersection for each ray, generates the shadow ray and computes
the final color by using the functions defined in the module Blinn-Phong.


### BackendRayTrace

Diagrams distinguish between backends with the use of type family. Type family
are Haskells extension to support ad-hoc overloading of data types. Type family are
parametric types that are specialized representations based on the type parameters. The
type of the scenes indicate which backend is used e.g. which function based on
the name and the parameter signature.

The module BackendRayTrace defines an instance of Diagrams backend type families
and a function “renderRTree” that extracts scene informations.
These are used to translate the scene information to a data structure that is
used in the rendering.

### CmdLine

Sometimes we want to specify the details about the diagram at the start of the program.
For example the output file, image format, and size of the diagram. The Diagrams
CmdLine module provides creation of a command-line interface that supports standard
options as well as easy customization for additional parameters. The abstraction allows
by simply defining the rendering backend and the interpretation of the command line
arguments to use these features.

## Comparison with POV-Ray

A comparison between a rendering of POV-Ray and the implemented raytracer is
given in \autoref{fig:snowman}. Both render the same scene with the image resolution 800 $\times$ 600
and were executed on a machine with

* CPU: intel core i7 920.
* GPU: AMD Readon HD 6970.
* OS: Ubuntu 14.04.4 LTS.

\begin{figure}[htbp]
\begin{minipage}[b]{0.5\linewidth}
\centering
\includegraphics[width=.8\linewidth]{snow.png}
\caption{Rendering of own raytracer.
   Rendering time was 6,1175 seconds.}
\label{fig:snow1}
\end{minipage}
\hspace{0.5cm}
\begin{minipage}[b]{0.5\linewidth}
\centering
\includegraphics[width=.8\linewidth]{snowPOV.png}
\caption{Rendering of POV-Ray.
  Rendering time was 0.920 seconds.}
\label{fig:sub2}
\end{minipage}
\caption{A comparison between the implemented raytracer and POV-Ray using 4 cores.}
\label{fig:snowman}
\end{figure}

# Conclusion and Further Work

In this thesis we provided an alternative Backend for the DSL Diagrams. The Backend
is a native renderer which implements the basic ray tracing algorithm. It supports
different kinds of primitives as described in \autoref{primitives}. The implementation
also provides a command-line Backend, which allows to define animations via Diagrams.
It exploits functional properties to simplify modification of data structures and certain
functions as described in \autoref{modules}. This removes the dependency on
POV-Ray from Diagrams.

Despite the fact that the DSL Diagrams is flexible and powerful in describing vector
graphics, this cannot be utilized without a certain understanding of Haskell and its
abstraction system.

Ray tracing can be optimized in various ways. It would be interesting to use acceleration
data structures to get a logarithmic scene complexity. It would also be interesting to
extend the implementation to use different devices for the computations. Also extending
the DSL Diagrams to allow more light types, different material properties, textures
(function that changes the color of a primitive per point) and more primitives.

The implemented ray tracer does not use advanced rendering techniques. It does not
consider reflections, refractions and area light. As shading technique is used the well
known Blinn-Phong model which approximates certain light behavior.
Advanced methods like path tracing, instant radiosity, Monte Carlo ray tracing, can be
added to the implementation for rendering more realistic images.
