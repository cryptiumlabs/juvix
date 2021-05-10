Intermediary form to check stratification for compatibility with the abstract algorithm.

## Syntax

Let erased core terms be $t$ and core types be $T$.

Define elementary affine core formulae as $A, B ::=\ T\ |\ !A$.

\begin{figure}[H]
\begin{align*}
T &::= ∗_i\ & \text{sort $i$} \\
&\ \ \ \ \ \ |\ \kappa \in K & \text{primitive type} \\
&\ \ \ \ \ \ |\ (x : S) → T\ & \text{dependent function type} \\
&\ \ \ \ \ \ |\ (x : S) ⊗ T\ & \text{dependent multiplicative conjunction type} \\
&\ \ \ \ \ \ |\ (x : S)\ \&\ T\ & \text{dependent additive conjunction type} \\
&\ \ \ \ \ \ |\ T \parr T & \text{non-dependent multiplicative disjunction type} \\
&\ \ \ \ \ \ |\ !T & \text{bang-type} \\
\end{align*}
\end{figure}

Define elementary affine core terms as $u, v ::= t\ |\ !u\ |\ \bar{!}u$.

Elementary affine core operates on an annotated version of erased core but requires preservation of a type mapping such that the types of free variables can be looked up.

## Typing rules

As core, except:

- Usage annotations erased
- All variables must be linear
- Added weakening rule
- Different rules for contraction & promotion
- Variables can occur twice in additive conjunction (but no more)

\begin{figure}[H]
\caption{Typing rules for EAL}

\begin{prooftree}
\AxiomC{}
\RightLabel{var}
\UnaryInfC{$x:A ⊢ x:A$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ t:B$}
\RightLabel{weak}
\UnaryInfC{$Γ, x:A ⊢ t:B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ_1 ⊢ t_1:A ⊸ B$}
\AxiomC{$Γ_2 ⊢ t_2:A$}
\RightLabel{app}
\BinaryInfC{$Γ_1,Γ_2 ⊢ (t_1 t_2):B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ,x:A ⊢ t:B$}
\RightLabel{abst}
\UnaryInfC{$Γ ⊢ λx.t:A ⊸ B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ_1 ⊢ t_1:\ !A_1, ..., Γ_n ⊢ t_n:\ !A_n$}
\AxiomC{$x_1:A_1, ..., x_n:A_n ⊢ t:B$}
\RightLabel{prom}
\BinaryInfC{$Γ_1, ..., Γ_n ⊢ !t[\bar{!}t_i/x_i]:\ !B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$x_1:\ !A, ..., x_n:\ !A, ∆ ⊢ t:B$}
\RightLabel{contr}
\UnaryInfC{$x:\ !A, ∆ ⊢ t[x/x_1,...,x_n]:B$}
\end{prooftree}

\end{figure}

## Box placement inference

Adapted from previous work [@feasible-algorithm-typing-eal].

Why / notes

- Boxes are unintuitive to write, add syntactic bureaucracy
- Inference will sometimes fail. In this case, programmer can be informed and suggested how else to write their function, or alternatively the bespoke encoding route can be taken.
- Most terms one would want to compute (especially in smart contracts) are in the elementary complexity class
- Allows better box placement than programmer might choose, compiler can pick from set of typeable EAL terms & instantiate with fewest number / optimally positioned boxes

Define:

1. Formulae $A,B ::=\ α\ |\ A ⊸ B\ |\ !A\ |\ ∀α.A$.
1. Pseudoterms $t,u ::= x\ |\ λx.t\ |\ (t)u\ |\ !t\ |\ \bar{!}t$.
1. Restricted pseudoterms
    1. $a ::= x\ |\ λx.t\ |\ (t)t$
    1. $t ::= !^m a$ where
        1. $m \in \mathbb{Z}$
        1. $!^ma =\ !...!\ (m\ times)\ a\ if m ≥ 0$
        1. $!^ma = \bar{!}...\bar{!}\ (m\ times)\ a\ if\ m < 0$

\begin{theorem}
All EAL-typable terms can be converted into restricted pseudo-terms (for proof see the paper).
\end{theorem}

### Box paths

Let $t$ be a pseudo-term and $x$ be a particular occurrence of a free or bound variable in $t$.

Define the **path** as an ordered list of the occurrences of $!$ and $\bar{!}$ enclosing $x$, more formally:

\begin{align}
path(x, x)        = nil \\
path(t_1 t_2, x)  = path(t_i, x)\ \text{where}\ t_i\ \text{contain}s\ x\ \\
path(λy.t, x)     = path(t, x) \\
path(!t, x)       = \ ! :: path(t, x) \\
path(\bar{!}t, x) = \ \bar{!}t :: path(t, x)
\end{align}

To-do: changes for full core, should be trivial.

Define the **sum** of a path $s(p)$ as:

\begin{align}
s(nil) = 0 \\
s(! :: l) = 1 + s(l) \\
s(\bar{!} :: l) = -1 + s(l)
\end{align}

Define the **well-bracketed** condition, mapping pseudo-terms to booleans, where $\le$ is the prefix relation on lists, for a pseudo-term $t$ as:

\begin{align}
∀l \le path(t, x), s(l) \ge 0\ \text{for any occurrence of a variable}\ x\ \text{in}\ t \\
∀x \in FV(t), s(path(t, x)) = 0\ \text{(zero sum paths for free variables)}
\end{align}

Define the **well-scoped** condition, mapping pseudo-terms to booleans, for a pseudo-term $t$ as:

\begin{align}
∀t_i \in subterms(t), well-bracketed(t_i)
\end{align}

\begin{theorem}
If $t$ is a EAL-typed term, $t$ is well-bracketed and well-scoped.
\end{theorem}

Let an EAL **type assignment** for a pseudo-term $t$ be a map $Γ'$ from free & bound variables of $t$ to EAL formulae.

Extend that map to a partial map $Γ$ from subterms of $t$ to EAL formulae as:

\begin{align}
Γ(!u) =\ !A\text{ if }Γ'(u) = A \\
Γ(\bar{!} u) = A\text{ if }Γ'(u) =\ !A\text{, undefined otherwise} \\
Γ(λx.u) = A ⊸ B\text{ if } Γ'(x) = A, Γ'(u) = B \\
Γ(t_1 t_2) = B\text{ if }Γ'(t_2) = A\text{ and }Γ'(t_1) = A⊸B\text{, undefined otherwise}
\end{align}

To-do: changes for full core, trivial except dependent types: function result type is dependent, bangs work the same way.

Let $(t, Γ)$ be a pair of a pseudo-term $t$ and an assignment $Γ$. $t$ satisfies the **typing condition** if:

\begin{align}
Γ(t_i)\text{ is defined for all subterms }t_i\text{ of }t \\
\text{ for any variable }x\text{ of at least 2 occurrences, }Γ(x) =\ !B\text{ for some }B
\end{align}

\begin{theorem}
If $t$ is an EAL-typed term and $Γ$ is an associated assignment then $(t, Γ)$ satisfies the typing condition.
\end{theorem}

\begin{theorem}
If $(t, Γ)$ satisfies the typing condition and $u$ is a subterm of $t$, then $(u, Γ)$ also satisfies the typing condition.
\end{theorem}

\begin{theorem}
If $t$ is a pseudo-term and Γ an assignment such that $t$ is well-bracketed and well-scoped, and $(t, Γ)$ satisfies the typing condition, then $t$ is typable in EAL with a judgement $∆ ⊢ t : A$ such that $Γ(t) = A$ and $∆$ is the restriction of $Γ$ to the free variables of $t$.
\end{theorem}

*Proof*

Enumerate the bracketing, scope, and typing conditions as (i), (ii), and (iii) respectively. Proceed by induction on the pseudo-term $t$:

1. $t = x$ is trivial
1. $t = λx.u$
    1. $u$ satisfies the first part of the bracketing condition since $t$ does
    1. $u$ satisfies the second part of the bracketing condition since $t$ satisfies the scope condition for $x$
    1. $u$ then trivially satisfies (ii), (iii), so by induction we have in EAL* $∆ , x : A ⊢ u : B$ where $Γ(x) = A, Γ(u) = B$
    1. Apply the abstraction rule to get the judgement for $t$
1. $t = t_1 t_2$
    1. Subterm $t_1$ satisfies conditions (i), (ii), (iii), hence by induction $∆_1 ⊢ t_1 : A_1$, where $Γ(t_1) = A_1$ and $∆_1$ is the restriction of $Γ$ to the free variables of $t$
    1. Subterm $t_2$ satisfies conditions (i), (ii), (iii), hence by induction $∆_2 ⊢ t_2 : A_2$, where $Γ(t_2) = A_2$ and $∆_2$ is the restriction of $Γ$ to the free variables of $t$
    1. As $t$ satisfies the typing condition $(iv)$ $A_1$ is of the form $A_1 = A_2 ⊸ B_1$
    1. If $t_1$ and $t_2$ share a free variable $y$, as $t$ satisfies the typing condition we have $Γ(y) = !B$
    1. Rename in $t_1$ and $t_2$ the free variables that they have in common, accordingly
    1. Apply an application rule followed by a contraction rule to get the judgement for $t$
1. $t = \bar{!} u$
    1. $t$ does not satisfy the bracketing condition (i) in the first prefix, so this case is invalid
1. $t =\ !u$
    1. By the boxing lemma, $t$ can be written as $t = !v [x_1 := \bar{!}u_1, ... x_n := \bar{!}u_n]$, where $FV(v) = {x_1 ... x_n}$ and $path(v, x_i)$ is well-bracketed for all $x_i$
    1. Let $y$ be an occurrence of a variable in $u_i$
        1. $path(t, y) =\ ! :: path(v, x_i) :: \bar{!} :: path(u_i, y)$
        1. $path(v, x_i)$ is well-bracketed, so $path(u_i, y)$ satisfies the bracketing condition and $u_i$ satisfies (i).
        1. Since $t$ satisfies (ii) and (iii), $u_i$, a subterm of $t$, also satisfies (ii) and (iii).
        1. Therefore there exists an EAL* derivation $∆_i ⊢ u_i : A_i$, where $A_i$ = $Γ(u_i)$, for $1 \le i \le n$.
    1. Now examine $v$
        1. Since $t$ satisfies the bracketing condition, by the boxing lemma, $v$ satisfies (i) and all free variables in $v$ have exactly one occurrence, so as $t$ satisfies (ii) $v$ does also.
        1. Let $Γ'$ be defined as $Γ$ but $Γ'(x_i)$ = $Γ(\bar{!}u_i)$ for $1 \le i \le n$.
            1. If $y$ occurs more than once in $v$ then it also does in $t$, hence $Γ(y) =\ !A$ so $Γ'(y) =\ !A$.
            1. If $v_1 v_2$ is a subterm of $v$ then $v'_1 v'_2$, where $v'_i = v_i [x_1 := \bar{!}u_1, ... x_n := \bar{!}u_n]$, is a subterm of $t$ and $Γ'(v'_i)$ = $Γ(v_i)$
            1. As $(t, Γ)$ satisfies (iii), so does $(v, Γ')$
            1. $Γ(u_i) = A_i$ and $Γ(\bar{!}u_i)$ is defined, so $A_i =\ !B_i$ and $Γ'(x) = B_i$
            1. As $v$ satisfies conditions (i), (ii), and (iii), by induction there exists an EAL* derivation $∆, x_1 : B_1, ..., x_n : B_n ⊢ v : C$ where $C = Γ'(v)$.
        1. If $u_i$ and $u_j$ for $i /= j$ have a common free variable $y$, as $t$ satisfies the typing condition $Γ(y) =\ !B$.
        1. Rename the free variables in common to the $u_i$s, apply a (prom) rule to obtain the judgements on $u_i$ and the judgement on $v$
        1. Then apply (contr) rules separately for the final judgement $∆' ⊢ t :\ !C$, concluding the proof

To-do: update for full core. Should be trivial, except possibly dependent type case.

### Decoration

Consider the **declaration problem**:

Let $x_1 : A_1, ..., x_n : A_n ⊢ M : B$ be a simply-typed term. Do there exist EAL decorations $A'_i$ of the $A_i$ for $1 \le i \le n$ and $B'$ of $B$ such that $x_1 : A'_1, ..., x_n : A'_n ⊢ M : B'$ is a valid EAL judgement for M?

### Parameterisation

Define **parameterised restricted pseudo-terms** as restricted pseudo-terms but with unique integer indices for each free parameter: $a ::= x\ |\ λx.t\ |\ t\ t$, $t ::= !^n a$ where $n$ is a fresh index chosen monotonically over $Z$. Given a parameterised pseudo-term $t$ denote by $par(t)$ the set of its parameter indices. An instantiation $φ$ maps $t$ to a restricted pseudo-term by instantiating each indexed parameter $n$ with the integer demarcation $φ(n)$.

Define **parameterised types** $A ::= !^n\ α\ |\ !^n (A ⊸ A)$ with $n$ a fresh index chosen monotonically over $Z$. Denote by $par(A)$ the set of parameters of $A$. If $φ$ instantiates each index $n$ with an integer demarcation $φ(n)$, $φ(A)$ is defined only when a non-negative integer is substituted for each parameter (per the type formulae of EAL). Define the size $|A|$ as the structural size of the underlying simple type.

Analogously to EAL type assignments for pseudo-terms consider parameterised type assignments for parameterised pseudo-terms with values parameterised types, and simple type assignments for lambda terms with values simple types. Let $Σ$ be a parameterised type assignment for a parameterised pseudo-term $t$. Denote by $par(Σ)$ the parameter set occurring in parameterised types $Σ(x)$, for all variables $x$ of $t$. Let $φ$ be an instantiation for $par(Σ)$ which associates a non-negative integer with each indexed parameter. Then define the map $φΣ$ by $φΣ(x) = φ(Σ(x))$. When defined, this map is an EAL type assignment for $φ(t)$. Define the size $|Σ|$ as the maximum of $|Σ(x)|$ for all variables $x$.

Define the erasure map $(.)^-$ for parameterised pseudo-terms and parameterised types analogously to those for pseudo-terms and EAL types. Given a lambda term $M$ there exists a unique parameterised pseudo-term $t$, up to renaming of the indices, such that $t^- = M$. Denote $t$ by $\bar{M}$ and call it the **parameter decoration** of $M$. Note that $|\bar{M}|$ is linear in $|M|$. Given a simple type $T$, its **parameter decoration** $\bar{T}$ is defined analogously. Finally, given a simple type assignment $Θ$ for a lambda term $t$, with values simple types, define its parameter decoration $\bar{Θ}$ point-wise by taking $\bar{Θ}(x) = \bar{Θ(x)}$, where all decorations are taken with disjoint parameters.

Thus the decoration problem is reduced to the following instantiation problem:

Given a parameterised pseudo-term $t$ and a parameterised type assignment $Σ$ for it, does there exist an instantiation $φ$ such that $φ(t)$ has an EAL type derivation associated to $φΣ$?

To answer this question we will translate the bracketing, scope, and typing conditions into a system of linear constraints.

### Constraint generation

#### Bracketing & scope

Let $t$ be a parameterised pseudo-term. Define the **boxing constraints** for $t$ as the set of linear equations $C^b(t)$ obtained in the following way:

- Bracketing: for any occurrence of a variable $x$ in $t$, and any prefix $l$ of $path(t, x)$, add the inequation $s(l) \ge 0$. If $x \in FV(t)$, add the equation $s(l) = 0$.
- Scope: for any subterm $λx.v$ of $t$, for any occurrence $x_i$ of $x$ in $v$, add similarly the inequations expressing that $path(v, x_i)$ is well-bracketed.

#### Typing constraints

Define parameterised type unification as:

\begin{align}
U(!^m α, !^n α) = \{ m = n \} \\
U(!^m (A_1 ⊸ A_2), !^n (B_1 ⊸ B_2)) = \{ m = n \} ∪ U(A_1, B_1) ∪ U(A_2, B_2) \\
U(\_, \_) = \{ false \}
\end{align}

Let $Σ$ be a parameterised type assignment for a parameterised pseudo-term $t$. Extend $Σ$ to a partial map from subterms of $t$ to parameterised types as:

\begin{align}
Σ(!^n a) = !^m A \text{ if }Σ (a) = !^k A\\
Σ(λx.u) = !^m (A ⊸ B) \text{ if } Σ(x) = A, Σ(u) = B\\
Σ(u_1 u_2) = B \text{ if } Σ(u_1) = A ⊸ B \text{ and } Σ(u_2) = A
\end{align}

Define the **typing constraints** for $(t,Σ)$ as the set of linear inequations $C^{typ}(t,Σ)$:

- for any subterm of $t$ with the form $λx.u$ with $Σ(λx.u) = !^m (A ⊸ B)$, add the constraint $m = 0$
- for any subterm of $t$ with the form $u_1 u_2$ with $Σ(u_1) = !^m (A_1 ⊸ B_1)$ and $Σ(u_2) = A_2$ add the constraints $U(A_1, A_2) ∪ { m = 0 }$
- for any subterm of $t$ with the form $!^n u$ with $Σ(!^n u)$ = $!^m A$ and $Σ(u) = !^k A$, add the constraints $m = k + n$ and $m \ge 0$
- for any subterm of $t$ with the form $x$ where $x$ has at least two occurrences and $Σ(x) = !^m A$, add the constraint $m \ge 1$
- for any parameter $m$ in $par(Σ)$, add the constraint $m \ge 0$

To-do: update for full core. Changes:

- variables can occur once on each side of an additive conjunction without the constraint
- do we need a constraint for the value in dependent types? can it appear as a computational variable? e.g. a function from types to types (identity), then it would be computed with though. maybe we don't need to do anything here, but should reason about it

\begin{theorem}
Let $t$ be a parameterised pseudo-term and $Σ$ be a parameterised type-assignment for $t$ such that $Σ(t)$ is defined. Given an instantiation $φ$ for $(t, Σ)$, $φΣ$ is defined and $(φ(t), φΣ)$ satisfies the typing condition if and only if $φ$ is a solution of $C^{typ}(t, Σ)$.
\end{theorem}

### Constraint solution

Generated constraints are simple integer (in)equalities and can be solved in polynomial time. At present Juvix exports them to Z3.

Multiple solutions may exist, in which case the solution with the least number of box-annotations is selected.
