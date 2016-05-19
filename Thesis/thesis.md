% Ray Tracing Backend for Diagrams
% Alexander Lochmann


# 3D Scene

In this section we will introduce definitions that we use to describe 3D scenes.

## Color

Color will be represented with the RGB model. The RGB color model represents a
color by adding red, green and blue lights together. The intensity of these lights
is defined by values in the interval [0,1], where 0 stands for 0% and 1 for
100% intensity. Each light value is called channel.
E.g. Color 1 0 0 has a red channel of 100%, green channel of 0% and blue
channel 0%.

## Light

A point light is defined by the location and the color. It sends light in
all direction.

## Transformations

Imagine we want to describe a scene where a object changes its position
in each frame. Now to define this scene we would either need different objects
or we define transformations for the object, to represent the motion.
The advantage of transformations is that the scene gets more dynamic.
To define these transformations we first must introduce homogeneous coordinates.

### Homogeneous coordinates

Homogeneous coordinates represent $n-dimensional$ coordinates with $n+1$ values.
The $n+1$ value is conventionally denoted by $w$. The represented point can be
calculated by dividing the n-values by $w$. For $w = 0$ the point lies at
infinity. This allows to define all following transformation as matrices.


### Scaling matrix

The scaling matrix $S$ changes the size of the object. It is defined as

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
E.g. if $\vec{p} = (1, 1, 1)$ and $s_x = 5, s_y = 1, s_z = 0$ of scaling
matrix $S$ then $S\vec{p} = (5, 1, 0)$. If $s_x = s_y = s_z$ then the scaling
is called uniform.

### Rotation matrix

To rotate a object in 3D space we need to define by which axis we want to
rotate and the angle $\alpha$.

A rotation around the x-axis is defined as:

$$
  R_x := \left(
          \begin{array}{cccc}
              1   & 0          & 0           & 0 \\
              0   & \cos \alpha & -\sin \alpha & 0 \\
              0   & \sin \alpha & \cos \alpha  & 0 \\
              0   & 0          & 0           & 1
           \end{array}
       \right)
$$

A rotation around the y-axis is defined as:

$$
  R_y := \left(
          \begin{array}{cccc}
              \cos \alpha  & 0 & \sin \alpha & 0\\
              0           & 1 & 0          & 0 \\
              -\sin \alpha & 0 & \cos \alpha & 0 \\
              0           & 0 & 0          & 1
           \end{array}
       \right)
$$

A rotation around the z-axis is defined as:

$$
  R_z := \left(
          \begin{array}{cccc}
              \cos \alpha & -\sin \alpha & 0 & 0 \\
              \sin \alpha & \cos \alpha  & 0 & 0 \\
              0          & 0           & 1 & 0 \\
              0          & 0           & 0 & 1
           \end{array}
       \right)
$$

For the corresponding proof see \cite{kenn}.
For a rotation $R_o$ around a arbitrary axis $\vec{a}$, we use the defined
rotation matrices to align $\vec{a}$ with one of the axes, we denote
the used rotation $R_a$. Then the resulting rotation matrix is:

$$
  R = R_a^{-1}R_oR_a
$$

### Translation matrix

The translation matrix is defined as

$$
T := \left(
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

with associativity we can verify that the matrix\  $M$ has the same effect

$$
  M\vec{p} = TRS\vec{p}
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

### Sphere

The unit sphere, centered at the origin, can be represented by
radius $r = 1$.
For all points $\vec{p}$ of the sphere follows $||\vec{p}|| = r$,
where $||\cdot||$ is the Euclidean norm.

### Box
A box aligned with the axes can be defined with two vectors
  $$\vec{v_1} := \begin{pmatrix}
          x_{min}, & y_{min}, & z_{min}
       \end{pmatrix} \\
  \vec{v_2} := \begin{pmatrix}
           x_{max}, & y_{max}, & z_{max}
      \end{pmatrix}
  $$
where $v_1$ represents the starting interval of each axis and
$v_2$ the end of the interval. For all points $\vec{p} = (x, y, z)$ of the box
follows $x \ge x_{min}, x \le x_{max}, y \ge y_{min}, y \le y_{max},
z \ge z_{min}, z \le z_{max}$.

### Cylinder

The infinity cylinder aligned at the z-axis can be represented with
radius $r \in \mathbb{R}$. For all points $\vec{p} = (x,y,z)$
follows $x^2 + y^2 = r^2$. A render of a cylinder is illustrated in
\autoref{fig:cylinder}.

![Cylinder.\label{fig:cylinder}](primCylinder.png)

For image size, see: <http://www.imagemagick.org/discourse-server/viewtopic.php?t=21076>


### Cone

  \begin{minipage}{\dimexpr\linewidth-2\fboxsep-2\fboxrule}
    \begin{wrapfigure}{L}{.2\textwidth}
    \centering
    \includegraphics[width=.2\textwidth,
     height=.25\textwidth]{cone.png}
    \end{wrapfigure}
    A cone which length is infinite, for short infinity cone,
    that is aligned at the z-axis can be defined by a base and a top cap, \\
    where the center of the base cap is at the origin and the center of the
    top cap at $\vec{p_2} = (0,0,1)$. We define the 2 radii, \\
    $r_1 \in \mathbb{R}$ the radius of the base cap and \\
    $r_2 \in \mathbb{R}$ the radius of the top cap. \\
    With these we can calculate the half-angle $\alpha = r_1 - r_2$ and the
    apex $\vec{p_a}= (0, 0, r_1 / \alpha)$. \\
    For all points $\vec{q}=(x, y, z)$ of the cone follows
    $$\cos^2 \alpha * (x^2 + y^2) - \sin^2 \alpha * (z - r_1 / \alpha) = 0$$
  \end{minipage}

We truncate the cone and the cylinder because of the infinity length by
the planes $p1$ and $p2$. The origin is a point of $p1$ and
$\vec{o} = (0 ,0 ,1)$ is a point of $p2$. The vector $\vec{d} = (0 ,0 ,1)$
is orthogonal to both planes. Every point that does not lies between these 2
planes will not be considered a point of the cone or cylinder.

### Graphical representation

* The unit sphere:\\
  \begin{minipage}{\dimexpr\linewidth-2\fboxsep-2\fboxrule}
    \centering
    \includegraphics[scale=.25]{primSphere.png}
  \end{minipage}
  \\
* The unit box, rotated by 45 degree on the y-axis: \\
  \begin{minipage}{\dimexpr\linewidth-2\fboxsep-2\fboxrule}
    \centering
    \includegraphics[scale=.25]{primBox.png}
  \end{minipage}
  \\
* The cone:\\
  \begin{minipage}{\dimexpr\linewidth-2\fboxsep-2\fboxrule}
    \centering
    \includegraphics[scale=.4]{primCone.png}
  \end{minipage}
  \\
* The cylinder:\\
  \begin{minipage}{\dimexpr\linewidth-2\fboxsep-2\fboxrule}
    \centering
    \includegraphics[scale=.4]{primCylinder.png}
  \end{minipage}
  \\


# Comments

* Give (mathematical) definitions
* Use "follows" correctly
* Use `git diff`!
* Use uniform enumerations, e.g. "translating, rotating, and scaling"
* Give full definitions if possible, i.e. do not "truncate" things afterwards
* Refer to scientific papers
* Use Markdown figures and refer to them in the text where appropriate
