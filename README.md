# Investigations around a possible central limit theorem for the oriented Thompson group $\vec{F}$

## The Thompson group $F$
The Thompson group $F$ admits an infinite presentations 
<p align="center">
$\langle x_0, x_1, ...| x_n x_l = x_l x_{n+1}$ for all $l < n\rangle$
</p>
see [CFP, B].

Every element of $F$ can be represented by pairs of binary trees with the same number of leaves and such pairs are called binary tree diagrams. 
The representation is non-unique, but there exists a unique minimal representive, that is with minimal number of leaves.

## The oriented subgroup $\vec{F}$
The oriented subgroup $\vec{F}$ was introduced by V. Jones in [Jo14] to produce oriented knots/links and later studied by Golan and Sapir. 
After that, in [A1] (see also [A2]) it was proved an Alexander type theorem for $\vec{F}$, that is every oriented knot/link can be produced by a suitable element of $\vec{F}$.

## A 'chromatic' state and its moments
Consider the linear functional $\varphi: \mathbb{C}[F] \to \mathbb{C}$ defined as
<p align="center">
 $\theta(g):=\frac{{\rm Chr}_{\Gamma(g)}(2)}{2}$ for any $g\in F$.
</p>
 
 When evaluated at an element of $F$, this functional returns $1$ if the element belongs to $\vec{F}$, and $0$ otherwise.

In this project, we examine the elements
<p align="center">
 $a_n:=\frac{x_n+x_n^{-1}}{\sqrt{2}} \qquad$       $s_n:=\frac{a_0 + ... + a_{n-1}}{\sqrt{n}}$.
</p>


We wrote some code to compute $(\sqrt{2 n})^k \theta(s_n^k)\in \mathbb{N}$ for all $n$, $k$ in $\mathbb{N}_0$.
We call this quantity 'unnormalized moment'. The code comes in two versions: in Python and C++.
These are contained in group_operations.py and c_group_operations.cpp

## Conclusions

It turned out that a central limit theorem for the chromatic state is not possibile. 
More details are contained in [A3]

## Bibliography
[A1] V. Aiello, On the Alexander Theorem for the oriented Thompson group F. Algebraic & Geometric Topology 20.1 (2020): 429-438.

[A2] V. Aiello, An introduction to Thompson knot theory and to Jones subgroups. accepted for publication in J. of Knot Theory and its Ramifications (2022).

[A3] V. Aiello, An extension of Krishnan's central limit theorem to the Brown-Thompson groups, preprint.

[B] J. Belk, Thompson's group F. Ph.D. Thesis (Cornell University).  preprint arXiv:0708.3609 (2007).

[CFP]
J.W. Cannon, W.J Floyd,   W.R. Parry, 
Introductory notes on Richard Thompson's groups.
L'Enseignement  Mathématique
42 (1996): 215-256

[GS] Golan, Gili, and Mark Sapir. On Jones' subgroup of R. Thompson group F. Journal of Algebra 470 (2017): 122-159.

[Jo14] V.F.R. Jones, Some unitary representations of Thompson's groups $F$ and $T$. J. Comb. Algebra 1 (2017), 1-44.

