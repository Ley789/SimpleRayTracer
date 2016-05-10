\renewcommand{\vec}[1]{\mathbf{#1}}

# 3D Scene

In this section we will introduce definitions that we use to describe 3D scenes
in our ray tracer.

## Primitives

We define a number of shapes that can be represented in the euclidean space. Every shape is considered hollow.

* sphere: a sphere, centered at the origin, can be represented with radius $r \in \mathbb{R}$, then for all points $\vec{p}$ of the sphere follows $||\vec{p}|| = r$ where $||\cdot||$ is the euclidean norm.

* box: a box aligned with the axes can be defined with 2 vectors
$$\vec{v_1} := \begin{pmatrix}
          x_{min}, & y_{min}, & z_{min}
       \end{pmatrix} \\
\vec{v_2} := \begin{pmatrix}
           x_{max}, & y_{max}, & z_{max}
      \end{pmatrix}
$$
where $v_1$ represents the starting interval of each axis and $v_2$ the end of the interval.

* Cone: a infinity cone aligned at the z-axis, base cap at the origin, top cap centered at $\vec{p_2} = (0,0,1)$, $r_1 \in \mathbb{R}$ is the radius of the base cap and $r2 \in \mathbb{R}$ the radius of the top cap, then the half-angle $\alpha = r_1 - r_2$, apex $\vec{p_a}= (0, 0, r_1 / \alpha)$, then for all points $\vec{q}=(x, y, z)$ of the cone follows
$$
  cos^2 \alpha * (x^2 + y^2) - sin^2 \alpha * (z - r_1 / \alpha) = 0
$$


* Cylinder: the infinity cylinder aligned at the z-axis can be represented with radius $r \in \mathbb{R}$, then for all point $\vec{p} =(x,y,z)$ follows $x^2 + y^2 = r^2$.

For the cone and the cylinder holds that they will be truncated at the planes with origin $\vec{o_1} = (0 ,0 ,0)$ and $\vec{o_2} = (0 ,0 ,1)$ and the orthogonal vector to the origins $\vec{d} = (0 ,0 ,1)$.
