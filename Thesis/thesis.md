% Ray Tracing Backend for Diagrams
% Alexander Lochmann


# 3D Scene

In this section we will introduce definitions that we use to describe 3D scenes.

## Color

A color is represented as vector $\vec{c} = (r,g,b)$, with values in
range of [0,1]. The components are called channels, where each channel
represents light intensity with, $r$ as the red, $g$ as the green and
$b$ as the blue channel. We use the RGB color model to calculate the resulting
color. E.g. $\vec{c} =(1, 0, 0)$ has a red channel of 100%, green channel of
0% and blue channel 0%, so the resulting color is full intensity red.

## Light

A light is a source that emits a color at a defined position in a defined
direction with a defined decrease of illumination per distance.
A point light sends light in all direction.
Direction light is a point light with no decrease illumination.

## Transformations

A transformation is a function that takes a vector $\vec{p} = (x_1,...,x_n)$
and returns a $\vec{q} = (y_1,...,y_n)$. In the next chapters we will
introduce homogeneous coordinates and then some important transformations.

### Homogeneous coordinates

Given a vector $\vec{p}=(x_1,...,x_n)$ the vector
$\vec{q}=(x_1 * w, ..., x_n * w, w)$ with $w \ne 0$ is a homogeneous vector
of the vector $\vec{p}$. We can see for every scalar $t \in \mathbb{R}$ and
$t \ne 0$ holds that $t*\vec{q}$ is a homogeneous vector of $\vec{p}$.
Representing coordinates of $\mathbb{R^3}$ as homogeneous vectors/coordinates
yields to simplification of the transformations. In the section Transformations
every point is considered a homogeneous point.

### Scaling matrix

The scaling matrix $S$ is defined as

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

where the indices stands for the scaling in the corresponding axis.
Multiplying every point of a object with a scaling matrix will resize
the object.
E.g. if $\vec{p} = (1, 1, 1, 1)$ and $s_x = 5, s_y = 1, s_z = 0$ of scaling
matrix $S$ then $S\vec{p} = (5, 1, 0, 1)$. If $s_x = s_y = s_z$ then the scaling
is called uniform.

### Rotation matrix

To rotate a object in 3D space we need to define by which axis we want to
rotate and the angle $\alpha$.

A rotation defined as:

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

where the indices indicate the axis by which we rotate. Every combination of
rotation matrices is also called a rotation matrix.
For the corresponding proof see \cite{kenn}.
Multiplying every point of a object with a rotation matrix will rotate
the object by \alpha.
To rotate a object on a arbitrary axis $\vec{p}$ by the rotation $R_o$,
we align $\vec{p}$ with one of the defined axes by using combinations
of the defined rotation matrices. We denote the used rotation matrix with $R_a$.
Afterwards we undo the rotation done to align the axis with $R_a^{-1}$.
We combine these steps to one rotation matrix

$$
  R = R_a^{-1}R_oR_a
$$

### Translation matrix

The translation matrix is defined as

$$
  T(x,y,z):= \left(
                \begin{array}{cccc}
                  0 & 0 & 0 & t_x \\
                  0 & 0 & 0 & t_y \\
                  0 & 0 & 0 & t_z \\
                  0 & 0 & 0 & 1
                \end{array}
              \right)
$$

where the indices describe the translation it the corresponding axis.

### Transformation matrix

Instead of scaling, rotating and then translating a given point $p$, we can use
the associativity property of matrices. We can create a matrix that has the
same result as the single transformations. This transformation matrix will
be defined as
$$
  M := TRS
$$

with associativity we can verify that the matrix $M$ has the same effect

$$
  M\vec{p} = TRS\vec{p}
$$

## Ray

A ray is defined as point $\vec{o}$ and a direction $\vec{d}$. Where
$\vec{o}$ represents the origin of the ray and $\vec{d}$ the direction.
The ray travels through all points of the set

$$
 \{ \vec{o} + t * \vec{d} | x \in \mathbb{R}, x > 0 \}
$$


## Primitives

We define a number of shapes, called primitives, that can be represented in
the euclidean space. Every shape is defined by the set of the points of its
surface.

### Sphere

The unit sphere, which is centered at the origin is defined by the set

$$
  \{p | p \in \mathbb{R^3}, ||p|| = 1 \}
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
  \{\vec{p}=(x,y,z)| p \in \mathbb{R^3}, x^2 + y^2 = r,1 \ge z \ge 0 \}
$$

A render of a cylinder is illustrated in
\autoref{fig:cylinder}.

![Cylinder.\label{fig:cylinder}](primCylinder.png)

For image size, see: <http://www.imagemagick.org/discourse-server/viewtopic.php?t=21076>


### Cone

A cone aligned at the z-axis, with length 1, base cap, centered at the origin,
with radius $r_1 \in \mathbb{R}$, top cap, centered at $\vec{p_2} = (0,0,1)$,
with radius $r_2 \in \mathbb{R}$ is defined by the set

$$
  \{\vec{p}=(x,y,z)| p \in \mathbb{R^3},
  \cos^2 \alpha * (x^2 + y^2) - \sin^2 \alpha * (z - r_1 / \alpha) = 0,
  1 \ge z \ge 0\}
$$

where the half-angle $\alpha = r_1 - r_2$

### Graphical representation


SEE CYLINDER


# Comments

* Give (mathematical) definitions
* Use "follows" correctly
* Use `git diff`!
* Use uniform enumerations, e.g. "translating, rotating, and scaling"
* Give full definitions if possible, i.e. do not "truncate" things afterwards
* Refer to scientific papers
* Use Markdown figures and refer to them in the text where appropriate
* Sketch out structure before writing and refine
