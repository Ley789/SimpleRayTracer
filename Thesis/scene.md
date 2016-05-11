\renewcommand{\vec}[1]{\mathbf{#1}}

# 3D Scene

In this section we will introduce definitions that we use to describe 3D scenes
in our ray tracer.

## Primitives

**MF: Your headline is "primitives", but you never use the word "primitive"
in this section. Explain it!**

We define a number of shapes that can be represented in the euclidean space.
Every shape is considered hollow.

* Sphere: A sphere, centered at the origin, can be represented by
  radius $r \in \mathbb{R}$.
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

