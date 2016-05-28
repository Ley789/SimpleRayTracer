% Ray Tracing Backend for Diagrams
% Alexander Lochmann


# 3D Scene

In this chapter we will introduce definitions that we use to describe 3D scenes.

## Vector notation

We will introduce notation conventions which we use in this thesis.
The multiplication of a vector $\vec{v}$ with a scalar $t$ is denoted as
$\vec{p} * t = t * \vec{p}$.

The standard scalar product of vectors $\vec{p}$ and $\vec{q}$ is denoted as
$<\vec{p},\vec{q}>$, where $<\vec{q}> = <\vec{q},\vec{q}>$.

The euclidean norm of a vector $\vec{q}$ is denoted as $||\vec{q}||$.


## Homogeneous coordinates

Given a vector $\vec{p}=(x_1,...,x_n)$, the vector
$\vec{q}=(x_1 * w, ..., x_n * w, w)$ with $w \ne 0$ is a homogeneous vector
of the vector $\vec{p}$. It follows that for every scalar $t \in \mathbb{R}$ and
$t \ne 0$, $t*\vec{q}$ is a homogeneous vector of $\vec{p}$. A homogeneous
vector is called a point if and only if $w \ne 0$. A homogeneous vector is called
a direction if and only if $w = 0$.

Representing coordinates of $\mathbb{R}^3$ as homogeneous vectors
simplifies transformations because points and directions have different
properties. In this thesis every point/direction is considered a homogeneous
point/direction except for the definitions in section \autoref{primitives} and
if not stated different.


## Color

A color is represented as vector $\vec{c} = (r,g,b)$, with values in
range of [0,1]. The components are called channels, where each channel
represents light intensity with $r$ as the red, $g$ as the green and
$b$ as the blue channel. We use the RGB color model to calculate the resulting
color.

E.g. $\vec{c} =(1, 0, 0)$ has a red channel of 100%, green channel of
0% and blue channel 0%, so the resulting color is red with full intensity.


## Camera

A camera is defined by 4 vectors.
The position of the camera $\vec{p}$, where $\vec{p}$ is a point.
The forward vector $\vec{f}$, where $\vec{f}$ is a direction.
The right vector $\vec{r}$, where $\vec{r}$ is a direction.
The up vector $\vec{u}$, where $\vec{u}$ is a direction.

To get a better understand of these vector see section \autoref{projection}.

## Viewing plane

The viewing plane matrix with $n$ rows and $m$ columns is defined as

$$
  V_{ij} = (-f(i,n), f(j,m))  
$$

where $1 \le i \le n$, $1 \le j \le m$ and $f(x,y)= ((x - 1)/ (y - 1)) - 0.5$.
The rows and columns of the viewing plane define the resolution.

## Light

A light is defined by a position and color c.

We assume that the light sends light of the specified color uniformly in all
directions.


## Ray

A ray is defined as a tuple $(\vec{o}, \vec{d})$, where $\vec{o}$ is called
the position and $\vec{d}$ the direction of the ray.
The ray travels through all points of the set

$$
 \{ \vec{o} + t * \vec{d} | t \in \mathbb{R}, t > 0 \}
$$


## Primitives

We define a number of shapes, called primitives, that can be represented in
the euclidean space. Every shape is defined by the set of the points of its
surface.

**TODO: create pictures of primitives in coordinate system and include them**

### Sphere

The unit sphere, which is centered at the origin, is defined by the set

$$
  \{p | p \in \mathbb{R}^3, ||p|| = 1 \},
$$

where $||\cdot||$ is the Euclidean norm.

### Box

A box aligned with the axes and all four sides of length 1 is defined by the set

$$
  P_x(0) \cup P_x(1) \cup P_y(0) \cup P_y(1) \cup P_z(0) \cup P_z(1)
$$

where

$$
  P_x(v) := \{(x,y,v) | 0 \le x,y \le 1, x,y \in \mathbb{R}\}
$$
$$
  P_y(v) := \{(v,y,z) | 0 \le y,z \le 1, y,z \in \mathbb{R}\}
$$
$$
  P_z(v) := \{(x,v,z) | 0 \le x,z \le 1, x,z \in \mathbb{R}\}  
$$

### Cylinder

The cylinder aligned at the z-axis, with length 1 and radius $r \in \mathbb{R}$
is defined by the set

$$
  \{\vec{p}=(x,y,z)| p \in \mathbb{R}^3, x^2 + y^2 = r,1 \ge z \ge 0 \}
$$

A render of a cylinder is illustrated in
\autoref{fig:cylinder}.

![Cylinder.\label{fig:cylinder}](primCylinder.png)

For image size, see: <http://www.imagemagick.org/discourse-server/viewtopic.php?t=21076>


### Cone

A cone aligned at the z-axis, with length 1, base cap $\vec{p_1}$, centered at
the origin, with radius $r_1 \in \mathbb{R}$, top cap, centered at
$\vec{p_2} = (0,0,1)$, with radius $r_2 \in \mathbb{R}$ is defined by the set

$$
  \{\vec{p}=(x,y,z)| p \in \mathbb{R}^3,
  \cos^2 \alpha * (x^2 + y^2) - \sin^2 \alpha * (z - r_1 / \alpha) = 0,
  1 \ge z \ge 0\}
$$

where $\tan \alpha = r_1 - r_2 / ||\vec{p_2} - \vec{p_1}|| = r_1 - r_2$.

## Transformations

A transformation is a function that takes a vector $\vec{p} = (x_1,...,x_n)$
and returns a $\vec{q} = (y_1,...,y_n)$. In the next sections we will
introduce a group of transformations.


### Scaling matrix

The scaling is defined as

$$
  S(s_x,s_y,s_z):= \left(
          \begin{array}{cccc}
              s_x & 0   & 0   & 0 \\
              0   & s_y & 0   & 0 \\
              0   & 0   & s_z & 0 \\
              0   & 0   &  0  & 1
           \end{array}
       \right)
$$

where the indices stands for the scaling in the corresponding axes.
A scaling matrix is a result of the scaling function defined above and
multiplications of 2 scaling matrices.
If $s_x = s_y = s_z$ then the scaling is called uniform.
Multiplying every point of a primitive with a scaling matrix resizes
the primitive.

For example, given $\vec{p} = (1, 1, 1, 1)$ and a scaling matrix
$S(5, 1, 0) = S'$, then $S'\vec{p} = (5, 1, 0, 1)$.

### Rotation matrix

To rotate a primitive in 3D space we need to define by which axis
and which angle $\alpha$ to rotate.

A rotation is defined as:

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
                  \right)
$$

where the indices indicate the axis by which we rotate and $\alpha$ the angle.
A rotation matrix is a result of the rotation function defined above and
multiplications of 2 rotation matrices.
For a corresponding proof see \cite{kenn}.
Multiplying every point of a primitive with a rotation matrix will rotate
the primitive.

To rotate a point $\vec{p}$ on a arbitrary axis $\vec{a}$ by the angle $\alpha$,
we first find a rotation matrix $R_a$ which aligns the axis $\vec{a}$ with one
of the defined axes (x-,y- or z-axis). Next we apply the rotation
$R_o(\alpha)R_a\vec{p} = \vec{p'}$, where $R_o$ is the defined rotation matrix
of the aligned axis. Afterwards we undo the rotation done to align the axis
with $R_a^{-1}$. This leads to the rotated point $\vec{p''} = R_a^{-1}\vec{p'}$.
We can combine these steps to one rotation matrix

$$
  R = R_a^{-1}R_o(\alpha)R_a
$$

### Translation matrix

A translation is defined as

$$
  T(x,y,z):= \left(
                \begin{array}{cccc}
                  1 & 0 & 0 & t_x \\
                  0 & 1 & 0 & t_y \\
                  0 & 0 & 1 & t_z \\
                  0 & 0 & 0 & 1
                \end{array}
              \right)
$$

where the indices describe the translation in the corresponding axes.
A translation matrix is a result of the translation function defined above
and multiplications of 2 translation matrices.

### Transformation matrix

We define a transformation matrix $M$ as multiplications of a
translation $T'$, rotation $R'$ and scaling matrix $S'$.

$$
  M := T'R'S'
$$

Every combination of translating, rotating and
scaling matrices can be represented as transformation matrix.

Proof: Given a matrix $L$ which is a result of multiplications
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

To extract the rotation matrix we first need to state a important property
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
\begin{split}
  s_x &= \sqrt{(r_{11} * s_x)^2 + (r_{21} * s_x)^2 + (r_{31} * s_x)^2} \\
      &= \sqrt{ r_{11}^2 * s_x^2 + r_{21}^2 * s_x^2 + r_{31}^2 * s_x^2} \\
      &= \sqrt{ s_x^2 * (r_{11}^2 + r_{21}^2 + r_{31}^2)} \\
      &= \sqrt{ s_x^2} * \sqrt{r_{11}^2 + r_{21}^2 + r_{31}^2} \\
      &= \sqrt{ s_x^2} = s_x
\end{split}
$$

we know that $\sqrt{r_{11}^2 + r_{21}^2 + r_{31}^2} = 1$ because of the property
of rotation matrices. The other components of the scaling matrix can be extracted
analog. Now we can create the scaling matrix $S'$.

We also see that we can extract the scaling components for the matrix $S'$
form arbitrary ordered multiplications of scaling and rotation matrices.

The remaining matrix $R'$ can be obtained by multiplying the inverse scaling
matrix with $L''$, where $L''$ is the matrix $L$ but with the last column
replaced by $\vec{v}=(0, 0, 0, 1)^T$

$$
  R' = L'' * S'^{-1}
$$
\qedhere

## Material property

A material property is a vector $\vec{p} = (a, d, s, r)$ which values are in
the interval $[0,1]$. The components are called coefficients whit $a$ as the
ambient, $d$ as the diffuse, $s$ as the specular and $r$ as the rough
coefficient.

## Object

A object is a 4 tuple $(pr,M ,\vec{c}, vec{p})$, where $pr$ is a primitive,
$M$ is a transformation matrix, $\vec{c}$ is a color and $\vec{p}$ is a
material property.


# Rendering

In the field of 3D computer graphics the process of generation a image from
a defined scene description is called rendering. Also the result of this process
is a rendering.

In this chapter we shortly introduce rasterization, because
its the most used method for real time applications. After that we introduce
ray tracing and at the end of the chapter we will compare both methods.

## Rasterization

To render a scene with rasterization, we execute following steps:

* Calculate the position, color and various attributes of the primitives.
* Convert the primitives into fragments, which are stored in a raster image.
  This raster image stores location, color, depth and other informations.
* Calculate the final color of each pixel based on the raster image.

For a introduction on this topic see
**TODO add cite to the book:

https://www.ics.uci.edu/~gopi/CS211B/opengl_programming_guide_8th_edition.pdf**

## Ray Tracing

Ray tracing is a rendering technique, which idea is to trace the path of rays
emitted from a camera that travel through pixels of a image. By intersecting the
rays with each object of a scene we can determine the visible object for
each ray. The visible object is the one with the closest intersection.

For each pixel corresponding to the ray we can calculated the color with the
properties(location, material, ...) of the closest object and the scene.
These can implicate the generation of new rays to simulate effects like
reflection and refractions. There are several ray tracing techniques that
share the same basic algorithm.

### History

Some techniques for shading machine rederings of soilds is a paper of
Arthur Appel published in 1968 \cite{appel} it is the first approach
to ray tracing. He represented light rays as mathematical lines and checked
if there is a intersection with an object. After a intersection the light rays
aren't traced further, also known as Ray Casting. These process doesn't considered
shadows or reflections.

Ray tracing is a extension of Appels algorithm that was introduced by Whitted
in 1980 \cite{whitt}. This extension considers shadow and reflections by
tracing rays/lines after the intersection. After a intersection a ray can
generate 3 new rays, the shadow ray, reflection ray and refraction ray. These
rays are called secondary rays. The starting point of a shadow ray is
the intersection point and its direction leads to a light source. This is
used to determine if the light source influences the object at the intersection
point. For example if an object is in between the shadow ray and the light source
the intersected point will not be illuminated and will be represented as shadow.
Reflection and refraction rays are traced further, which makes the algorithm
recursive.

Distribution ray tracing was introduced by Cook in 1984 \cite{cook}. This method
increases realism of the image by using probability distributions for specific
effects and increases the number of generated rays to approximate the result.
For example generating multiple shadow rays for area light sources to represent
soft shadows.

Path tracing was introduces by Kajiya in 1986 \cite{kaji}. Applying
distributing rays not only to specific effects like shadows, but for the shading
of all diffuse surfaces. With these the illumination of the lights with all
object can be simulated correctly. This method is also called monte carlo
ray tracing, because it uses random samples to compute the image.

### Basic Algorithm

The basic concept of a ray tracing algorithm is to find intersections of a ray
with a scene consisting of a set of geometric primitives efficiently \cite{wald}.
The ray, as defined in the section \autoref{Ray}, can have additional parameters
$t_{min}$ and $t_{max}$, which specifies the interval of $t$ used to define
the set of all points of the ray. In other words it specifies the minimum
and the maximum distance of a ray.

At this level the algorithm can be split in 3 tasks.
The most fundamental task is to find the closest intersection. The second task,
also called visibility/occlusion test, is to check if there are any intersection
of a ray with an object. The last task is find all intersections of a ray.
To check for any intersections is slightly simpler as checking for the
closest, so there are algorithms that are more efficient in this case. For
example the occlusion test is used for shadow rays \cite{wald}.
In ray tracing only one condition for a primitive must apply, which is a
be a function that can calculate a intersection between the primitive and a ray.
That means primitives can be of various types, from simple geometric shapes like
sphere, cubes, trianles,..., to complex parametric patches like the
Bézier patches and other complex shapes as long as there exist a intersection
function. The flexibility of primitives allows to represent shapes with full
accuracy. Although using multiple kinds of primitives does not limit the kinds
of scenes that can be rendered. Like mentioned in the chapter \autoref{Rasterization}
most real time applications uses rasterization techniques to render a image and
most of them only uses triangles as primitives.

Testing every primitive in the scene for a intersection with a ray produce
the correct result, but the computation time extends with each ray and each
primitives. For complex scenes it is necessary reduce the set of primitives
that the ray could intersect. To preserve consistency its common to use
acceleration data structures, e.g. grids and kd-trees \cite{copy}.


### Performance

At the beginning of modern computer graphics only simple scenes were used and
interactive graphic applications weren't established. The scenes didn't
aim to be realistic. Ray tracing is computational intents that aims to
simulate realistic behavior. These are part of the reasons why rasterization is
the well-established rendering technique for interactive applications and ray
tracing is still seldom in this field.

However the demand of more detailed scenes, larger scenes and more realism
leads to the argument that ray tracing will outperform rasterization at some
point because of the logarithmic scene complexity \cite{wald}.

Ray tracing is usually used for offline rendering due to the fact that it
is computational intents. The crucial factors for the ray tracing algorithm
are:

* The amount of rays. Consider a image with a resolution of 800 x 800 and
  without counting the secondary rays. This results in a total amount of
  640.000 rays. Considering the secondary rays the total number of rays
  would increase even further. Depending on the scene complexity the set
  of primary rays may only be small part compared to the set of all rays.

* Scene complexity. Reducing the set of primitives for a ray with acceleration
  data structures is the most important optimization in ray tracing because
  it can give the algorithm a logarithmic behavior in scene complexity \cite{copy}.


The common crucial factors lead to common optimizations. For the first fact,
the amount of rays, its sufficed to reduce the number of rays to reduce
the computation time. This can be archived by reducing the primary rays(smaller
resolution) or reducing the secondary rays. The second fact, the scene complexity,
can be optimized by using the mentioned acceleration data structures. Also
using optimized intersection functions leads to reduced computation time. \\
Ray tracing is knows as embarrassing parallel problem. That means to execute the
algorithm in parallel is trivial because the result for each ray can be calculated
independently. The optimization of ray tracing is a topic of interest since the
invention of the algorithm \cite{copy}.


## Ray Tracing compared with rasterization

Advantages of ray tracing:

  * Ray tracing in known for the realistic images in high quality.
  * The algorithm is embarrassing parallelism.
  * The algorithm scales logarithmic with the scene complexity.
  * Complex effects like shadows, reflection and refractions can be simulated
    and represented correctly (no approximations).
  * Allows physically correct representations.

Advantages of rasterization:

  * Established method with approximations of physical effects.
  * Part of the rasterization algorithm is already executed by the hardware.
  * In general fast computation time.


Rasterization based rendering needs to use  a number of approximations
for specific effects, e.g. shadow maps to simulate shadows. Without specific
criteria rasterization may not simulation certain effects correctly.

Ray tracing can use approximations to save computation time but it is nor
required. Whereas rasterization must use them for certain effects.

# DSL

## Diagrams

**Why using diagrams instead of pov ray?**

## POV-Ray


# Implementation

In this chapter we introduce fundamental elements that are needed to implement
the basic ray tracing algorithm. First we discuss the intersection functions
for the defined primitives in section \autoref{primitives}. After that we will
show a method to keep the intersection functions simple even with
transformations. We will describe how transformations influence normal
vectors. Next we describe the generation of primary and how they lead to
different projections. At the end we introduce a well known shading
algorithm to approximate light behavior.

## Intersection functions

The intersection functions are based on euclidean space. So we first need to
convert a ray to euclidean coordinates. Given a ray $R$ with origin at point
$\vec{o} = (o_x,o_y,o_z,w)$ and direction $\vec{d} = (d_x, d_y, d_z, 0)$, then
the ray with the origin $\vec{o'}= (o_x / w, o_y / w, o_z / w)$ and the direction
$\vec{d'}= (d_x, d_y, d_z)$ represents $R$ in euclidean space.

In the following subsections we denote $\vec{o}=(x_o, y_o, z_o)$ as origin and
$\vec{d} = (x_d, y_d, z_d)$ as direction of a ray and for all points and
directions follows that they are denoted in euclidean space.

### Sphere intersection

We see from the definition of the sphere \autoref{sphere} that we have to solve
the equation

$$
  <\vec{o} + t*\vec{d}, \vec{o} + t*\vec{d}> = 1
$$

$\implies$

$$
  <\vec{o},\vec{o}> + t * 2 * <\vec{o}, \vec{d}> + t^2 * <\vec{d}, \vec{d}> - 1 = 0
$$

This leads to following quadratic equation:

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a},
$$

where $a = <\vec{d},\vec{d}>$, $b=2*<\vec{o},\vec{d}>$ and $c=<\vec{o},\vec{o}>$.

### Box intersection

To get the intersection between a ray and a box we check the boundaries. We
will show this for the x-component, for the other components it works analog.

$$
  \begin{split}
  tmin_x = (0 - x_o) * (1 / d_x ) \\
  tmax_x = (1 - x_o) * (1 / d_x )
  \end{split}
$$

Now we have the minimum for the x-component. After calculating the minimum and
the maximum values for the other components we take the maximum of all minimums,
$tmin = max(tmin_x, tmin_y, tmin_z)$, and the minimum of all maximums,
$tmax = max(tmax_x, tmax_y, tmax_z)$. If $tmin > tmax$ or $tmin <= tmax <0$ then
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
  t = \frac{<\vec{n}, \vec{p} - \vec{o}>}{<\vec{n}, \vec{d}>}  
$$

### Cylinder intersection

For a cylinder aligned on a arbitrary line $\vec{p_a} + \vec{v_a} * t$, a
point on the cylinder $\vec{q}$ and radius $r$ holds
$<\vec{q} - \vec{p_a} - <\vec{v_a},\vec{q} - \vec{p_a}> * \vec{v_a}> - r^2 = 0$.

The definition of cylinder in section \autoref{cylinder} allows us to simplify
the equation. We substitute the point on the cylinder $\vec{q}$ with the ray

$$
  <\vec{o} + t * \vec{d} - (0,0, z_o + t * z_d)> - r^2 = 0
$$

$\implies$

$$
  (x_o + t * x_d)^2 + (y_o + t * y_d)^2 - r^2 = 0
$$

$\implies$

$$
  t^2 * (y_d^2 + x_d^2) + t * 2 * (x_o * x_d + y_o * y_d) + x_o^2 + y_o^2 - r^2 = 0
$$

This leads to following quadratic equation

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a},
$$

where $a = y_d^2 + x_d^2$, $b = 2 * (x_o * x_d + y_o * y_d)$ and
$c = x_o^2 + y_o^2 - r^2$.

To get the intersection of a cylinder first solve the quadratic equation of
the cylinder and also intersect with the planes $p'$, which includes
the base cap, and $p''$, which includes of the top cap. After we
check that $1 \ge z \ge 0$ holds for the point $\vec{q} = (x,y,z)$ which is the
result of the cylinder equation. Next we check that the plane intersections
are in the range of the caps by verifying $||\vec{q_1}|| \le r^2$ and
$||\vec{q_2}|| \le r^2$, where $\vec{q_1}$ is the intersection of the ray with
plane $p'$ and $\vec{q_2}$ the intersection with the plane $p''$.

### Cone intersection

For a cone aligned on a arbitrary line $\vec{p_a} + \vec{v_a} * t$ with apex
$\vec{p_a}$, center of the base cap at $\vec{p_1}$, center of the top cap at
$\vec{p_2}$, a point on the cone $\vec{q}$ and half-angle $\alpha$ holds
$\cos^2 \alpha <\vec{q} - \vec{p_a} - <\vec{v_a},<\vec{q} - \vec{p_a},\vec{v_a}>>
 - \sin^2 \alpha <\vec{v_a},\vec{q} - \vec{p_a}> = 0$,

where $\vec{p_a} = \vec{p_1} + r_1 * (\vec{p_2} - \vec{p_1})/(r_1 - r_2)$.

Same as for the cylinder, substitute the ray with $\vec{q}$ and solve the
equation for $t$. The definition of cone in section \autoref{cone} allows us
to simplify the resulting components for the quadratic equation

$$
  t = \frac{-b \pm \sqrt{b^2 - 4 * a* c}}{2 * a}
$$

with

$$
  \begin{split}
    a &= \cos^2 \alpha * (x_d^2 + y_d^2) - \sin^2 \alpha * z_d^2 \\
    b &= 2* \cos^2 \alpha * (x_d * x_o + y_d * y_o) - 2 * sin^2 \alpha * (z_o - z_d) \\
    c &= cos^2 \alpha * (x_o^2 + y_o^2) - sin^2 \alpha * (z_o - 1)^2,
  \end{split}
$$

To get the intersection of a cone first solve the quadratic equation of
the cone and also intersect with the plane $p'$ which includes the
base cap. After we check that $1 \ge z \ge 0$ holds for the point
$\vec{q} = (x,y,z)$ that is the result of the cone equation. Next we
check that the plane intersection is in the range of the cap by verifying
$||\vec{q_1}|| \le r^2$, where $\vec{q_1}$ is the intersection of the ray with
plane $p'$.

## Ray transformations

To keep our intersection functions simple, we transform the ray instead
of the primitives. Given a ray $r = \vec{o} + t * \vec{d}$, a transformation
matrix $M = TRS$ and a point $\vec{p}$, we see that

$$
  r = M\vec{p}
$$

$\implies$

$$
  M^{-1}r = M^{-1}M\vec{p} = I\vec{p} = \vec{p}
$$

Now we simplify the expression because a translation does not affect
a direction

$$
  M^{-1}(\vec(o) + t * vec{d}) = M^{-1}\vec{o} + S^{-1}R^{-1}\vec{d} * t
$$

Note that $T^{-1}\vec{d} =\vec{d}$ holds for all directions $\vec{d}$.
We see that we get the same result as applying the transformation on a point.

## Normal vectors

Normal vectors are unit vectors which are orthogonal to given surface at a given
point. In this subsection we show how to calculate normal vectors for the
primitives.

### Sphere normal

From the sphere definition in section \autoref{sphere} follows that for all
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
$\vec{p_a} = (0 ,0 , r_1 / (r_1 - r_2))$, then the normal vector is
$\vec{n} = (x / m , y / m , r / c * m)$, where the hypotenuse
$c = \sqrt{(r_1 / (r_1 - r_2))^2 + r_1^2}$ and $m = \sqrt{x^2 + y^2 + (r_1/c)^2}$.


### Normal vector transformation

Generally by applying a transformation $M$ to a point $\vec{p}$ with normal
$\vec{n}$ doesn't follow that the normal vector is transformed properly. For
example if we apply a non uniform scaling. So we need to find a transformation
that transforms our normal vector correctly. First we transform the normal vector
to a homogeneous direction. After we see that

$$
  <\vec{n}, \vec{p}> = \vec{n}^{\tr}\vec{p} = 0
$$

$\implies$

$$
  \vec{n}^{\tr} I \vec{p} = \vec{n}^{\tr} M^{-1} M\vec{p} = 0,
$$

where $M\vec{p}$ is our transformed point. So the normal vector of the
transformed point is

$$
  \vec{n'}^{\tr} = \vec{n}^{\tr} M^{-1}
$$

$\implies$

$$
  \vec{n'} = (\vec{n}^{\tr} M^{-1})^{\tr} = (M^{-1})^{\tr} \vec{n}
$$

with $M = TRS$ follows

$$
  \begin{split}
  (M^{-1})^{\tr} \vec{n} &= (S^{-1}R^{-1}T^{-1})^T \vec{n} \\
                   &= (T^{-1})^{\tr} (R^{-1})^{\tr} (S^{-1})^{\tr} \vec{n} \\
                   &= R S^{-1}\vec{n}
  \end{split}
$$

So the equation for the transformed normal vector is
$\vec{n'} = R S^{-1}\vec{n}$.


We used the property of rotation matrices that for every rotation matrix
$R$ follows that $(R^{-1})^{\tr} = R$.

## Projection

In the context of computer graphics, a projection transforms points in 3D
onto a plane with finite many points\cite{kevi}.
The way we generate the rays determines the kind of projection.

### Perspective

In a perspective projection each line is centered at the camera position \cite{kevi}.
Given a camera $c=(\vec{p},\vec{f},\vec{u},\vec{r})$ and a viewing plane $V$,
we define the matrix of rays that represents a perspective projection as

$$
    P_{ij} :=  (\vec{p}, \vec{f} + snd(V_{ij}) * \vec{r} + fst(V_{ij}) * \vec{u})
$$

where $snd((x,y)) = y$ and $fst((x,y)) = y$. The indices of the matrix
determine the position of the pixels in the resulting image.

### Orthographic

In a orthographic projection every ray is parallel to each other and has its
center in the view plane. Given a camera $c=(p,f,u,r)$ and a viewing plane $V$,
we define the matrix of rays that represents a orthographic projection as

$$
    O_{ij} :=  (\vec{f} + snd(V_{ij}) * \vec{r} + fst(V_{ij}), \vec{f})
$$

where $snd((x,y)) = y$ and $fst((x,y)) = y$. The indices of the matrix
determine the position of the pixels in the resulting image.

## Shading model

Shading defines the calculation of the output color for each pixel.
We use a slightly different shading model that is described in the book
\cite{kevi}, which is also known as Blinn-Phong shading model.

After generating the projection we intersect each ray with each objects
primitive and we consider only the closest intersection. If there is no
intersection, then the resulting color will be black. If we find
a intersection, we will use the following defined shading method.


### Ambient light

Simulating indirect illumination is very computation depending, a common
practice is to assume that illumination is constant throughout the scene.
This is called ambient illumination and it's not a good approximation, but it
provides some illumination for the parts that do not receive direct illumination
\cite{kevi}.

We calculate the ambient color of a object with the ambient coefficient $a$ of
the objects material property multiplied by the color $\vec{c_a} = (1, 1, 1)$.

### Diffuse light

To simulate direct illumination we first need to check if there is no object
between the intersection point and the light.

We do this by generating a ray $S$
with position $\vec{p}$ and direction $\vec{d} = \vec{l_p} - \vec{p}$,
where $\vec{p}$ is the intersection point and $\vec{l_p}$ is the position of the light.
Then we try to intersect $S$ with every object. If there is no intersection
or the nearest intersection is further away as $\vec{l_p}$, then the light
illuminates the object and we calculate the diffuse color with following
equation

$$
  \vec{c_d} = \vec{c} * d * \max (0, <\vec{l_d}, \vec{n}>),
$$


where $n$ is the normal vector of the surface at position $\vec{p}$, $d$ is the
diffuse coefficient of the objects material property, $\vec{c}$ the color of
the light and $\vec{l_d}$ is the normalized vector $\vec{d}$.

### Specular reflection

To simulate smooth, shinny objects we allow them to reflect light that's
concentrated around the direction of mirror-reflection \cite{kevi}.

For specular reflection we also need to check if there is no object between
the intersection point and the light. We assume that there in between, then
we calculate the specular reflection color with following equation


$$
  \vec{c_d} = \vec{c} * s * \max (0, <\frac{\vec{v} + \vec{l_d}}{||\vec{v} + \vec{l_d}||},
     \vec{n}>)^r,
$$

where $n$ is the normal vector of the surface at position $\vec{p}$, $s$ is the
diffuse coefficient, $r$ is the rough coefficient of the objects
material property, $\vec{c}$ the color of the light,
$\vec{l_d}$ is the normalized vector $\vec{d} = \vec{l_p} - \vec{p}$ and $\vec{v}$
is the negated direction of the ray that intersected the object.


### Multiple lights and final results

We get the final color by calculating the diffuse color and the specular
reflection color for each light, add them together, then add the
ambient color and finally multiply with the objects color.

# Integration

## Diagrams Backend


## Diagrams command line Backend

**TODO implemnt**

# Related Work

where is ray tracing used, some other techniques
acceleration data structures...

# Comments

* Give (mathematical) definitions
* Use "follows" correctly
* Use `git diff`!
* Use uniform enumerations, e.g. "translating, rotating, and scaling"
* Give full definitions if possible, i.e. do not "truncate" things afterwards
* Refer to scientific papers
* Use Markdown figures and refer to them in the text where appropriate
* Sketch out structure before writing and refine

# Bibliography
