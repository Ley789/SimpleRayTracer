% Ray Tracing Backend for Diagrams
% Alexander Lochmann


\renewcommand{\vec}[1]{\mathbf{#1}}

# 3D Scene

In this section we will introduce definitions that we use to describe 3D scenes
in our ray tracer.

## Color

Color will be represented with the RGB model. The RGB color model represents a
color by adding red, green and blue lights together. The intensity of these lights
are defined by values of the interval [0,1], where 0 stands for 0% and 1 for
100% intensity. Each light value is called channel.
E.g. Color 1 0 0 has a red channel of 100%, green channel of 0% and blue
channel 0%.

## Light

A point light is defined by the location and the color. It sends light in
all direction.

## Homogeneous coordinates

Homogeneous coordinates represent $n-dimensional$ coordinates with $n+1$ values.
The $n+1$ value is conventionally denoted by $w$. The represented point can be
calculated by dividing the n-values by $w$. For $w = 0$ the point lies at
infinity. This allows to define all following transformation as matrices.

## Scaling matrix

The scaling matrix $S$ is defined as

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


## Rotation matrix

Reading Paper

## Translation matrix

The translation matrix is defined as

$$
T := \left(
          \begin{array}{cccc}
              0 & 0 & 0 & x \\
              0 & 0 & 0 & y \\
              0 & 0 & 0 & z \\
              0 & 0 & 0 & 1
           \end{array}
       \right)
$$

## Transformation matrix

Instead of scaling, rotating and then translate a given point $p$, we can use
the associativity property of matrices. We can create a matrix that has the
same result as the single transformations. This transformation matrix will
be defined as
$$
  M := TRS
$$

with associativity we can verify that the matrix\  $M$ has the same effect

$$
  Mp = TRSp
$$

## Ray

A ray can is defined as point $\vec{o}$ and a direction $\vec{d}$. Where
$\vec{o}$ represents the origin of the ray and $\vec{d}$ the direction in
which the ray travels. The ray travels through all points of the set
$$
 \{ o + t * d | x \in \mathbb{R}, x > 0 \}
$$


## Primitives

We define a number of shapes, called primitives, that can be represented in
the euclidean space. Every shape is considered hollow.

* Sphere: The unit sphere, centered at the origin, can be represented by
  radius $r = 1$.
  For all points $\vec{p}$ of the sphere follows $||\vec{p}|| = r$,
  where $||\cdot||$ is the Euclidean norm.

* Box: A box aligned with the axes can be defined with two vectors
  $$\vec{v_1} := \begin{pmatrix}
          x_{min}, & y_{min}, & z_{min}
       \end{pmatrix} \\
  \vec{v_2} := \begin{pmatrix}
           x_{max}, & y_{max}, & z_{max}
      \end{pmatrix}
  $$
  where $v_1$ represents the starting interval of each axis and
  $v_2$ the end of the interval.
  **MF: This is completely unclear to me what you mean by these
  start/end intervals. Furthermore, what are the equations for the box?**

* Cone: An infinity cone aligned at the z-axis, base cap at the origin,
  top cap centered at $\vec{p_2} = (0,0,1)$,
  $r_1 \in \mathbb{R}$ is the radius of the base cap and
  $r_2 \in \mathbb{R}$ the radius of the top cap,
  then the half-angle $\alpha = r_1 - r_2$,
  apex $\vec{p_a}= (0, 0, r_1 / \alpha)$,
  then for all points $\vec{q}=(x, y, z)$ of the cone follows
  $$\cos^2 \alpha * (x^2 + y^2) - \sin^2 \alpha * (z - r_1 / \alpha) = 0$$
  **MF: Explain what is an infinity cone? The sentence in your description
  is very hard to parse because it is very long and sometimes uses "then"
  and sometimes not. Please make it a bit clearer, just as if you would
  explain it to me in person. Say it out loud.**

* Cylinder: The infinity cylinder aligned at the z-axis can be represented with
  radius $r \in \mathbb{R}$,
  then for all points $\vec{p} = (x,y,z)$ follows $x^2 + y^2 = r^2$.
  **MF: Sentence structure: The [...], then [...]. In English, a "then" is
  usually preceded by an "if", so do not write "then" without "if" before.**

The cone and the cylinder will be truncated at the planes
with origin $\vec{o_1} = (0 ,0 ,0)$ and $\vec{o_2} = (0 ,0 ,1)$
and the orthogonal vector to the origins $\vec{d} = (0 ,0 ,1)$.

**MF: What does it mathematically mean to "truncate"?**

**MF: Could you make a page where you show the unit sphere, box, cone
and cylinder graphically?**



**MF: Please try to not make the text longer than 80 characters per line.
This has the advantage that one can later see easier the differences
with `diff`.**
