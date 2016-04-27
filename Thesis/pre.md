# Predefinition

### Homogeneous coordinates

Still reading paper!!!


### Primitives

A computer-graphic system defines a set of types of shapes. A primitive of this system is a shape and it's type was defined by the system.
Every shape thats type is not part of the set must be approximated with primitives. For example, a sphere can visually be represented with triangles.

### Ray
A ray can mathematically be described as point $o$ and a direction $d$. Where $o$ represents the origin of the ray and $d$ the direction in which the ray travels. Every point that intersects with the ray can be calculated using the function:
$$
r(t): \Bbb R_\ge \rightarrow \Bbb R_\ge : t \mapsto o + t * d
$$

where $\Bbb R_\ge := \{ x | x \in \Bbb R, x \ge 0 \} $

### Scaling matrix

The scaling matrix $S$ defined as

$$
  S := \left(
          \begin{array}{cccc}
              s_x & 0   & 0   & 0 \\
              0   & s_y & 0   & 0 \\
              0   & 0   & s_z & 0 \\
              0   & 0   &  0  & 1
           \end{array}
       \right)
$$

where the indices stands for the scaling in the corresponding axis.

inverted Scale error fix program code see own written proof

### Rotation matrix

Reading Paper

### Translation matrix

With homogeneous coordinates the translation matrix can easily be defined as

$$
  Tr := \left(
          \begin{array}{cccc}
              0 & 0 & 0 & x \\
              0 & 0 & 0 & y \\
              0 & 0 & 0 & z \\
              0 & 0 & 0 & 1
           \end{array}
       \right)
$$

### Transformation matrix

Instead of scaling, rotating and then translate a given point $p$, we can use the associativity property of matrices. We can create a matrix that has the same result as the single transformations. This transformation matrix will be defined as
$$
  T := TrRS
$$

it is easy to verify that the matrix\  $T$ has the same effect

$$
  Tp = TrRSp
$$

### Visual plane

Die visuelle Ebene ist ein "finite plane, because the number of points are finite." Es ist die darzustellende Bildebene dessen Anzahl an Punkten gleich ist der Anzahl an Pixel des gerendereten Bildes. Die Ebene wird beschreiben durch einen Punkt $p$ der orthogonal auf dem ursprung der ebene zeigt und $x \in [-0.5 , 0.5]$ und $y \in [-0.5 , 0.5]$


### Camera

Eine Kamera wird definiert durch folgende 4 Vektoren
forward
right
up
position
wobei position die Poistion der Kamera festlegt, forward die distanz des visual planes und right/up den spect ratio

### Normal vector

Given a point $p$ on the surface of a primitive then the normal vector is a normalized vector thats orthogonal to the surface at the point $p$.
