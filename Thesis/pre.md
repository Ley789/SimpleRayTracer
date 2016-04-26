# Preliminaries

### Homogeneous coordinates

### Primitives

A computer graphic system defines a set of types of shapes.
A primitive of this system is a shape and its type was defined by the system.
Every shape thats type is not part of the set must be approximated with primitives. For example, a sphere can visually be represented with triangles.

### Ray
A ray can mathematically be described as point $o$ and a direction $d$. Where $o$ represents the origin of the ray and $d$ the direction in which the ray travels. Every point that intersects with the ray can be calculated using the function:
$$
r(t): \mathbb{R}_\ge \rightarrow \mathbb{R} _\ge : t \mapsto o + t * d
$$

where $$\mathbb{R}_\ge := \{ x | x \in \mathbb{R}, x \ge 0 \} $$


### Scaling matrix



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


inverted Scale error fix program code

### Rotation matrix



### Translation matrix

With homogeneous coordinates the translation matrix can easily be defined as:

$$
  T(x,y,z) := \left(
          \begin{array}{cccc}
              0 & 0 & 0 & x \\
              0 & 0 & 0 & y \\
              0 & 0 & 0 & z \\
              0 & 0 & 0 & 1
           \end{array}
       \right)
$$

### Transformation matrix

Anstatt einen gewählten punkt $p$ skalieren, rotieren und verschieben anhand der oben beschriebenen matrizen können wir die Assoziativität von Matrizen ausnutzen und dadurch eine Matrix erzeugen die den selben effect erziehlt wie die einzehlnen transformationen.

$$
  T := TrRS
$$

wobei ersichtlicht ist das folgendende behauptung gilt

$$
  Tp = TrRSp
$$

### Visual plane

Die visuelle Ebene ist ein "finite plane because the number of points are finite." Es ist die darzustellende Bildebene dessen Anzahl an Punkten gleich ist der Anzahl an Pixel des gerendereten Bildes. Die Ebene wird beschreiben durch einen Punkt $p$ der orthogonal auf dem ursprung der ebene zeigt und $x \in [-0.5 , 0.5]$ und $y \in [-0.5 , 0.5]$


### Camera and

Eine Kamera wird definiert durch folgende 4 Vektoren
forward
right
up
position
wobei position die Poistion der Kamera festlegt, forward die distanz des visual planes und right/up den spect ratio

### Normal vector

Given a point $p$ on the surface of a primitive then the normal vector is a normalized vector thats orthogonal to the surface at the point $p$.
