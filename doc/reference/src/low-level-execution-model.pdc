## Overview

Juvix translates the semantics of a term to equivalent semantics for an interaction system, consisting of node types, rewrite
rules, write-forward and read-back algorithms (for translating terms to and from nets, respectively),
where elementary-affine-typed terms are in the general case reduced using the oracle-free variant
of Lamping's optimal reduction algorithm.

Compared with previous interaction net interpreters for the lambda calculus utilising a static set
of node types and fixed rewrite rules, Juvix adds an additional degree of freedom:
the node types and rewrite rules of the interaction system can be generated at compile time and even
dynamically altered at runtime according to patterns of rewrite combinations and desired time-space complexity trade-offs.
Additional type data from the core language, such as exact variable usage counts provided by
the instantiation of quantitative type theory with the natural ring, are available to the interaction
system construction algorithm.

Also
- refl (equality) proofs in core language can be used by compiler, e.g. with total supply of a token = constant, for queries on the total supply the constant can be returned; more generally if two expressions are equal the compiler can choose which one to evaluate
- will be more effective if graph representation is persistent, instead of written / read-back each contract call. can be used for both code & data

- Define encoding $\phi (t)$ of term $t$ mapping to net $n$
- Define read-back function $\phi ^{-1}(n)$ mapping net $n$ to a term $t$, where $\phi^{-1}(\phi(t)) = t$ holds
- Define interaction system reduction function $\psi(n)$ mapping nets to nets, where $\phi^{-1}(\psi(\phi(t))) = reduce\ t$ where $reduce$ is as defined in the semantics of Juvix Core

## Interaction system encoding

### Basic lambda calculus

EAL term language $t ::= x\ |\ λx.t\ |\ (t u)\ |\ !t$.

EAL type $A ::= α\ |\ A\ ⊸\ A\ |\ !A$.

EAL-typed terms can be translated into interaction nets, in accordance with the sequent calculus typing rules, as the function $\phi$ as follows.

The EAL term is first erased to a simply-typed term, with EAL types and levels of subterms retained in a lookup table for reference during the translation.

**Abstraction** is applied to terms of the form $λx.t$ and type $A ⊸ B$.

\begin{tikzpicture}
  \inetbigcell[right = 30pt]{g}{$\phi (t)$}[4]
  \inetbigcell[below = 30pt]{l}{$λ$}[3]

  \axWire{g/1}{l/1}{$(arg)$}{}
  \cutWire{g.out}{l/2}{}{}
  \outwire[]{l.out}{$A ⊸ B$}
\end{tikzpicture}

Wiring of the argument $x$ varies depending on variable usage linearity:

**Weakening**: If $x$ does not appear in the body $t$, the $λ$ argument port is connected to an eraser.

\begin{tikzpicture}
  \inetbigcell[right = 30pt]{g}{$\phi (t)$}[4]
  \inode[]{e}{$⊗$}
  \inetbigcell[below = 30pt]{l}{$λ$}[3]

  \cutWire{e}{l/1}{$$}{}
  \cutWire{g.out}{l/2}{$$}{}
  \outwire[]{l.out}{$A ⊸ B$}
\end{tikzpicture}

**Linear / contraction**: If *x* appears once or more in the body $t$, the $λ$ argument port is connected to the occurrence(s). If there is more than one occurrence, usages will be shared by a set of fan nodes constructed by the application encoding.

\begin{tikzpicture}
  \inetbigcell[right = 30pt]{g}{$\phi (t)$}[4]
  \inetbigcell[below = 30pt]{l}{$λ$}[3]

  \axWire{g/1}{l/1}{}{}
  \cutWire{g.out}{l/2}{}{}
  \outwire[]{l.out}{$A ⊸ B$}
\end{tikzpicture}

**Application** is applied to terms of the form $(t_1 t_2)$ and type $C$.

\begin{tikzpicture}

  \inetbigcell[right = -25 pt]{g}{$\phi(t_1)$}[3]
  \inetbigcell[right = 25 pt]{h}{$\phi(t_2)$}[3]
  \inetbigcell[below = 40 pt]{a}{$@$}[3]

  \cutWire{g.out}{a/1}{}{}
  \cutWire{h.out}{a/2}{}{}
  \outwire[]{a.out}{$C$}

\end{tikzpicture}

For each free variable $x$ in $(t_1 t_2)$ occurring more than once, all occurrences of $x$ must be connected by a tree of fan-in nodes, each with a globally unique label (only one fan-in node is shown in the diagram).

\begin{tikzpicture}

  \inetbigcell[rotate = 180, above = 40 pt]{f}{\rotatebox[origin=c]{180}{$f_i$}}[3]
  \inetbigcell[right = -25 pt]{g}{$\phi(t_1)$}[3]
  \inetbigcell[right = 25 pt]{h}{$\phi(t_2)$}[3]
  \inetbigcell[below = 40 pt]{a}{$@$}[3]

  \cutWire{g.out}{a/1}{}{}
  \cutWire{h.out}{a/2}{}{}
  \outwire[]{a.out}{$C$}
  \inwire[]{f.out}{$x$}

  \axWire{g/1}{f/1}{}{}
  \axWire{h/1}{f/2}{}{}
\end{tikzpicture}

That ends the encoding rules for basic lambda terms.

#### Rewrite rules

The oracle-free abstract algorithm for optimal reduction operates on four node types: $λ$ (lambda), $@$ (application), $f_i$ (fan, with index $i$), and $⊗$ (eraser). Rewrite rules always operate only on primary port pairs and consist of two categories: **annihilation** rules, which remove nodes, and **commutation** rules, which create nodes.

\floatstyle{plain}
\restylefloat{figure}

\begin{figure}[H]
  \caption{Lambda-application annihilation (beta reduction)}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$@$}[5]
      \inetcell[rotate = 180, below = 50 pt]{l}{\rotatebox[origin=c]{180}{$λ$}}[5]
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \outwire[]{l/1}{c}
      \outwire[]{l/4}{b}
      \cutWire{a.out}{l.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \wirecross[]{a}{c}{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Fan-fan commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inetcell[rotate = 180, below = 50 pt]{b}{\rotatebox[origin=c]{180}{$f_j$}}[5]
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \outwire[]{b/1}{c}
      \outwire[]{b/4}{b}
      \cutWire{a.out}{b.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[rotate = 180]{a}{\rotatebox[origin=c]{180}{$f_j$}}[5]
      \inetcell[rotate = 180, right = 60 pt of a]{b}{\rotatebox[origin=c]{180}{$f_j$}}[5]
      \inetcell[below = 40 pt of a]{c}{$f_i$}[5]
      \inetcell[below = 40 pt of b]{d}{$f_i$}[5]
      \inwire[]{a.out}{a}
      \inwire[]{b.out}{d}
      \outwire[]{c.out}{b}
      \outwire[]{d.out}{c}
      \swire[0]{a/4}{c/1}{}{}
      \swire[0]{b/1}{d/4}{}{}
      \swire[0]{a/1}{d/1}{}{}
      \swire[0]{b/4}{c/4}{}{}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Fan-application commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inetcell[rotate = 180, below = 50 pt]{b}{\rotatebox[origin=c]{180}{$@$}}[5]
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \outwire[]{b/1}{c}
      \outwire[]{b/4}{b}
      \cutWire{a.out}{b.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[rotate = 180]{a}{\rotatebox[origin=c]{180}{$@$}}[5]
      \inetcell[rotate = 180, right = 60 pt of a]{b}{\rotatebox[origin=c]{180}{$@$}}[5]
      \inetcell[below = 40 pt of a]{c}{$f_i$}[5]
      \inetcell[below = 40 pt of b]{d}{$f_i$}[5]
      \inwire[]{a.out}{a}
      \inwire[]{b.out}{d}
      \outwire[]{c.out}{b}
      \outwire[]{d.out}{c}
      \swire[0]{a/4}{c/1}{}{}
      \swire[0]{b/1}{d/4}{}{}
      \swire[0]{a/1}{d/1}{}{}
      \swire[0]{b/4}{c/4}{}{}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Fan-lambda commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inetcell[rotate = 180, below = 50 pt]{b}{\rotatebox[origin=c]{180}{$λ$}}[5]
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \outwire[]{b/1}{c}
      \outwire[]{b/4}{b}
      \cutWire{a.out}{b.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[rotate = 180]{a}{\rotatebox[origin=c]{180}{$λ$}}[5]
      \inetcell[rotate = 180, right = 60 pt of a]{b}{\rotatebox[origin=c]{180}{$λ$}}[5]
      \inetcell[below = 40 pt of a]{c}{$f_i$}[5]
      \inetcell[below = 40 pt of b]{d}{$f_i$}[5]
      \inwire[]{a.out}{a}
      \inwire[]{b.out}{d}
      \outwire[]{c.out}{b}
      \outwire[]{d.out}{c}
      \swire[0]{a/4}{c/1}{}{}
      \swire[0]{b/1}{d/4}{}{}
      \swire[0]{a/1}{d/1}{}{}
      \swire[0]{b/4}{c/4}{}{}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Fan-fan annihilation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inetcell[rotate = 180, below = 50 pt]{b}{\rotatebox[origin=c]{180}{$f_i$}}[5]
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \outwire[]{b/1}{c}
      \outwire[]{b/4}{b}
      \cutWire{a.out}{b.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \wirestraight[]{a}{c}{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Eraser-lambda commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$λ$}[5]
      \inode[below = 40 pt]{b}{$⊗$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$⊗$}
      \inode[right = 20 pt]{b}{$⊗$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Eraser-application commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$@$}[5]
      \inode[below = 40 pt]{b}{$⊗$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$⊗$}
      \inode[right = 20 pt]{b}{$⊗$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Eraser-fan commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inode[below = 40 pt]{b}{$⊗$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$⊗$}
      \inode[right = 20 pt]{b}{$⊗$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Eraser-eraser annihilation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$⊗$}
      \inode[below = 40 pt]{b}{$⊗$}
      \vwire{a}{b}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
  \end{subfigure}
\end{figure}

### Linear connectives

#### Multiplicative conjunction

##### Constructor

##### Destructor

##### Rewrite rules

#### Multiplicative disjunction

Interaction net encoding of $a \parr b$:

1. Primary port as pair, to be connected to destructor ("join")
1. $2$ auxiliary ports, where:
    1. Two are for the subterms $a$ and $b$
1. Rewrite rule
    1. Erases the ⊗ node
    1. Creates a new ⊗ node, attaches its 2 auxiliary ports to a and b
    1. Attaches whatever the destructor was attached to to the new ⊗ node's primary port

This is structurally identical to the ⊗ encoding (perhaps we can simply erase the "join" destructor prior to runtime), but we should be able to place directives that inform the evaluator to evaluate the two subterms in parallel, as they are guaranteed not to share resources (no duplication required) and be completely disjoint subgraphs.

##### Constructor

##### Destructor

##### Rewrite rules

#### Additive conjunction

Interaction net encoding of $a \& b$:

1. Primary port as pair, to be connected to destructor ($fst$ / $snd$)
1. $3n + 2$ auxiliary ports where:
    1. Two are for the subterms $a$ and $b$
    1. $n$ are for the terms bound to free variables (resources) both subterms use
    1. $n$ are connected to the binding sites in a
    1. $n$ are connected to the binding sites in b
1. Rewrite rule
    1. If the destructor is $fst$ (vice versa if the destructor is $snd$):
        1. Connects the wires between the $n$ free variables and the $n$ binding sites in $a$
        1. Attaches erasers to the $n$ binding sites in $b$ and to $b$ itself
        1. Erases $\&$ node
        1. Attaches whatever destructor was connected to to $a$

That way no duplication of resources need occur, matching the linear logic semantics.

Note that this means reduction within $a$ and $b$, insofar as it depends on the values of the free variables, will not take place until the caller chooses which variant ($a$ or $b$) they want.

##### Constructor

##### Destructor

##### Rewrite rules

### Primitive constants

#### Constructors

\begin{figure}[H]
  \begin{tikzpicture}
    \inode[]{c}{$c$}
    \outwire[]{c}{$$}
  \end{tikzpicture}
\end{figure}

Primitive constants are encoded as simple custom nodes.

#### Rewrite rules

\begin{figure}[H]
  \caption{Constant-fan commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inode[below = 40 pt]{b}{$c \in C$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$c$}
      \inode[right = 20 pt]{b}{$c$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

\begin{figure}[H]
  \caption{Constant-eraser annihilation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$c \in C$}
      \inode[below = 40 pt]{b}{$⊗$}
      \vwire{a}{b}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
  \end{subfigure}
\end{figure}

### Primitive functions

To-do: we need curried functions for this case too. Should we just combine this with bespoke encoding?

#### Constructors

\begin{figure}[H]
  \begin{tikzpicture}
    \inetbigcell[below = 20pt]{f}{$f$}[5]
    \outwire[]{f.out}{$$}
    \outwire[]{f/1}{$$}
    \outwire[]{f/4}{$$}
  \end{tikzpicture}
\end{figure}

Functions $f$ of arity $n$ are encoded as custom nodes with $n - 1$ auxiliary ports.

#### Rewrite rules

\begin{figure}[H]
  \caption{Function-fan commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f_i$}[5]
      \inode[below = 40 pt]{b}{$f$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$f$}
      \inode[right = 20 pt]{b}{$f$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

To-do: this is wrong, deal with $n$ ports.

\begin{figure}[H]
  \caption{Function-eraser commutation}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f$}[5]
      \inode[below = 40 pt]{b}{$⊗$}
      \inwire[]{a/1}{a}
      \inwire[]{a/4}{d}
      \cutWire{a.out}{b.north}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{a}{$⊗$}
      \inode[right = 20 pt]{b}{$⊗$}
      \inwire[]{a}{a}
      \inwire[]{b}{d}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

Generalises to $n$ auxiliary ports - a new eraser node is attached to each.

\begin{figure}[H]
  \caption{Function-constant application}
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inetcell[]{a}{$f$}[5]
      \inetcell[rotate = 180, below = 50 pt]{l}{\rotatebox[origin=c]{180}{$c$}}[5]
      \inwire[]{a/1}{a}
      \cutWire{a.out}{l.out}{}{}
    \end{tikzpicture}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.05\textwidth}
    \huge{→}
  \end{subfigure}
  ~
  \begin{subfigure}[c]{0.1\textwidth}
    \begin{tikzpicture}
      \inode[]{r}{$r$}
      \inwire[]{r}{a}
    \end{tikzpicture}
  \end{subfigure}
\end{figure}

Where $r$ is the result of reducing $f c$ according to the defined rule $→_f$.

### Bespoke encoding

#### Functions

Consider a Core term $f$ of type $A ⊸ B$.

In the interaction net encoding compiler path, assuming EAL-typeability, we would encode this (if of form $λx.t$, for example) as:

\begin{tikzpicture}
\inetbigcell[right = 30pt]{g}{$\phi (t)$}[4]
\inetbigcell[below = 30pt]{l}{$λ$}[3]

\axWire{g/1}{l/1}{}{}
\cutWire{g.out}{l/2}{}{}
\outwire[]{l.out}{$A ⊸ B$}
\end{tikzpicture}

where $φ$ is the recursive interaction net translation function.

In the bespoke encoding path, we instead create a new node type $T$ and rewrite rule $R$ such that when the primary port of $T$ is connected to an application node to an argument $A$, we erase $T$, connect an eraser to $A$, and connect whatever the application node's primary port was connected to to a new subgraph which is equal to the encoding of $eval (f A)$.

$eval (f A)$ can then be implemented by native evaluation semantics which do not utilise interaction nets. For example, if the Core term in question is a tail-recursive numerical computation, it can be compiled to a native loop (possibly using SIMD).

Furthermore, we can safely encode non-EAL-typable terms this way, such as the Ackermann function, and they can safely interact with the rest of the interaction net (which must have been an EAL-typable term, treating the bespoke-encoded subterm as opaque).

The decision of whether or not to take the bespoke path can be made for all subterms of this form according to some heuristic (or possibly exact cost calculation) in the compiler.

#### Dealing with various types

Where $A$ and $B$ are both types which are encoded as primitive nodes (e.g. integers), this is trivial.

Where $A$ is a primitive type and $B$ is a function of some arity, of only primitive-typed arguments, which then returns a primitive type, this can be implemented as a sequence of node types $T, T', T''$, etc. which keep the curried arguments and eventually evaluate when all arguments are provided (or even when some are provided, there is a continuum of options here).

Where $A$ and/or arguments of $B$ are non-primitive types (e.g. functions), this becomes more complex, since we must convert between AST and interaction-net form during reduction.

More generally, with our Core term $f$ of type $A ⊸ B$, encoding $f$ through the bespoke path would result in a set of new node types $T_i$ with possible curried internal data, and a set of rewrite rules $R_i$, the first $i - 1$ of which just deal with currying (although again, there is a continuum of options, but let's leave that out for now), and the last one of which is interesting, let it be $R$.

$R$ must then cause, when connected to a primary port of an argument $A$:

- Erasure of $R_i$ (the prior node).
- Connection of an eraser to $A$.
- Creation of a new subgraph $φ (eval (f (read-back A))$, where $φ$ is the recursive interaction net encoding function (which might itself perform bespoke encoding, although we need to be concerned about runtime costs here), and $read-back$ is the read-back function from nets to Core terms, run starting at $A$ as the root node.
- Connection of the primary port of this subgraph to whatever $R_i$ was previously connected to.

This follows all the interaction net laws and should preserve semantics - but there are oddities:

- Read-back and (complex) encoding algorithms must be executed at runtime
- Read-back must happen over a term A which may be in the progress of parallel reduction

In general, we have no idea of the size (and corresponding read-back cost) of $A$, and it might be dependent on the order of reduction.

#### Datatypes

- Primitive types (integer, string, bytes) ~> node types w/data (same as constants)

\floatstyle{boxed}
\restylefloat{figure}

## Argument for correctness of the abstract algorithm

1. Define the level of a subterm
    1. level a a = 0
    1. level lam x . a b = level a b
    1. level a b c = level a c or level b c as appropriate
    1. level !a b = 1 + level a b
    1. level $\bar{!}a$ b = -1 + level a b
1. The level of a subterm is constant through beta reduction
    1. lam x . t $→$ lam x . t (trivial)
    1. x $→$ x (trivial)
    1. !x $→$ !x (trivial)
    1. ! $\bar{!}$ x $→$ x (trivial) (defined-ness guaranteed by well-bracketed property)
    1. (lam x . a) b $→$ a [ x := b ]
        1. If t was in a - trivial
        1. If t was in b - level a x = 0 by well-bracketed property, so level b t = level (a [ x := b ]) t
1. Map this level to nodes in the interaction net translation
    1. Think concentric boxes with natural number levels
1. Level of node does not change during reduction
    1. Beta reduction only connects nodes on same level
1. Levels can be chosen from contraction nodes of EAL type derivation
    1. Contraction nodes do not change level during proof-net reduction
    1. Also do not change level during abstract algorithm reduction
    1. If fans match label, must have originated from that level
    1. Algorithm is correct with EAL term subset since label indicates level of fan and level does not change according to EAL rules
    1. No loops in reduction of EAL-typable terms that would render labels underspecified

(needs pretty pictures)

## Argument for correctness of read-back

We wish to show that a read-back of an EAL-expression that has undergone zero or more reductions gives back the same result as normal evaluation up to alpha equality.

Before we can prove this result, we must first prove a few lemmas and theorems first as well talk about what fan in's with the same label imply.

The first lemma we wish to prove is the following

\begin{Lemma}{AST→Net has one free port}{AST->Net}
  Let $A$ be a valid BOHM term.
  \\ \\
  Now Consider the net encoding of $A$, let $N_a$ be this net.
  \\ \\
  Now let us consider any sub case of $A$, $L = P(n_1 \star \cdots \star n_k)$.
  \\ \\
  The node corresponding to $L$ looks.
  \\ \\
  TODO :: put ports on this images
  \begin{tikzpicture}
    \draw (0,0) circle (.8cm);
  \end{tikzpicture}
  \\ \\
  Where the labels in $N_a$ are the same as in $L$, with $n_{k+1}$ being an extra port which connects to the ADT above it.
  \\ \\
  For an ADT to be realized, we must have all arguments, so all these ports must be connected.
  Now, if $L$ is $A$, then we have one free port, however if it is not, then this free port must not be free but instead be connected to the ADT above it.
  \\ \\
  For free variables which have no $\lambda$ to connect to, a symbol node is created, maintaining the invariant.
  \\ \\
  $\therefore$ AST→Net has one free port
\end{Lemma}

Now we need to prove that evaluating this net does not change this fact

\begin{Theorem}{AST→Net→Eval has one free port}{AST->Net->Eval}
  Let $A = Net(AST)$.
  \\ \\
  By lemma \ref{Lm:AST->Net} $A$ has one free port.
  \\ \\
  We must now show $Eval(A)$ does not change the number of free ports.
  \\ \\
  We can show this by simply considering how every reduction rule works. For the sake of brevity, we will only consider the three variations of rewrite rules present in the BOHM system WLOG.
  \\ \\
  TODO :: Make the ports vary for all example with $\cdots$ drawn on the images
  \begin{tcolorbox}[title=Case 1 - Rewire, colframe=inner-box-1]
    Nodes that fall under this case:

    1. And↔T

    2. And↔F

    3. Or↔T

    4. Or↔F

    \begin{figure}[H]
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[]{a}{$g$}[5]
          \inetcell[rotate = 180, below = 50 pt]{l}{\rotatebox[origin=c]{180}{$f$}}[5]
          \inwire[]{a/1}{a}
          \inwire[]{a/4}{d}
          \outwire[]{l/1}{c}
          \outwire[]{l/4}{b}
          \cutWire{a.out}{l.out}{}{}
        \end{tikzpicture}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.05\textwidth}
        \huge{→}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \wirecross[]{a}{c}{b}{d}
        \end{tikzpicture}
      \end{subfigure}
    \end{figure}

    The wire connecting the main ports of node $f$ and node $g$ is eliminated.
    \\ \\
    The rest of the wires are simply rewired to each other, thus not altering the total number of free ports.
  \end{tcolorbox}

  \begin{tcolorbox}[title=Case 2 - Isolation, colframe=inner-box-1]
    Nodes that fall under this case:

    1. Cdr↔Cons

    2. Car↔Cons

    3. TestNil↔Cons

    4. IfThenElse↔T

    5. IfThenElse↔F

    \begin{figure}[H]
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[]{a}{$f$}[6]
          \inode[below = 40 pt]{b}{$g$}
          \inwire[]{a/1}{a}
          \inwire[]{a/3}{b}
          \inwire[]{a/5}{c}
          \cutWire{a.out}{b.north}{}{}
        \end{tikzpicture}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.05\textwidth}
        \huge{→}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inode[right = 20 pt]{b}{$⊗$}
          \inwire[]{b}{b}
          \draw (-1.5,-1.5) node{a}  .. controls (-1.5,1) and (0,-1.5) .. (0,1) node{c};
        \end{tikzpicture}
      \end{subfigure}
    \end{figure}

    Here we can see that like the first case the wire that connects the main ports are eliminated, and that $a$ and $c$ are rewired.
    \\ \\
    Additionally an eraser node is connected to the $b$, since the eraser node only has a main port, the number of free ports is not affected.
  \end{tcolorbox}

  \begin{tcolorbox}[title=Case 3 - Creation, colframe=inner-box-1]
    Nodes that fall under this case:

    1. TestNil↔Nil

    2. All nodes which ''curry'' and eventually become an Int node or T/F

    \qquad - add, not, mod, div, sub, more, noteq, eq, meq, less, leq. ↔ intlit

    3. Not↔T

    4. Not↔F

    5. Cdr↔Nil

    \begin{figure}[H]
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[]{a}{$f$}[6]
          \inode[below = 40 pt]{b}{$g$}
          \inwire[]{a/1}{a}
          \inwire[]{a/5}{c}
          \cutWire{a.out}{b.north}{}{}
        \end{tikzpicture}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.05\textwidth}
        \huge{→}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[]{a}{$g$}[6]
          \inwire[]{a/3}{a}
          \cutWire{a.out}{b.north}{}{}{}
        \end{tikzpicture}
      \end{subfigure}
    \end{figure}

    In this case, we take a node with $n$ ports and construct a node with $n-1$ ports. since the remaining ports are still wired to the same locations,
    the number of free ports does not change
  \end{tcolorbox}

  \begin{tcolorbox}[title=Case 4 - Duplication, colframe=inner-box-1]
    \begin{figure}[H]
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[]{a}{$f_i$}[5]
          \inetcell[rotate = 180, below = 50 pt]{b}{\rotatebox[origin=c]{180}{$a$}}[5]
          \inwire[]{a/1}{a}
          \inwire[]{a/4}{d}
          \outwire[]{b/1}{c}
          \outwire[]{b/4}{b}
          \cutWire{a.out}{b.out}{}{}
        \end{tikzpicture}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.05\textwidth}
        \huge{→}
      \end{subfigure}
      ~
      \begin{subfigure}[c]{0.1\textwidth}
        \begin{tikzpicture}
          \inetcell[rotate = 180]{a}{\rotatebox[origin=c]{180}{$a$}}[5]
          \inetcell[rotate = 180, right = 60 pt of a]{b}{\rotatebox[origin=c]{180}{$a$}}[5]
          \inetcell[below = 40 pt of a]{c}{$f_i$}[5]
          \inetcell[below = 40 pt of b]{d}{$f_i$}[5]
          \inwire[]{a.out}{a}
          \inwire[]{b.out}{d}
          \outwire[]{c.out}{b}
          \outwire[]{d.out}{c}
          \swire[0]{a/4}{c/1}{}{}
          \swire[0]{b/1}{d/4}{}{}
          \swire[0]{a/1}{d/1}{}{}
          \swire[0]{b/4}{c/4}{}{}
        \end{tikzpicture}
      \end{subfigure}
    \end{figure}
    Here we see the case for a fan-in node $f_i$ and some arbitrary node $a$. Both nodes get duplicated,
    however the number of free ports is preserved, as each node that is created is internally connected to the duplicated nodes.
    Additionally the four external ports are preserved among the four duplicated nodes.
  \end{tcolorbox}
  $\therefore$ since all cases are covered, $Eval(A)$ has only one free port.
\end{Theorem}

We now shall define one more definition before getting to our main theorem.


\begin{Definition}{Valid EAL-Net}{Valid EAL-Net}
  A \textcolor{definitions}{\textbf{Valid EAL-Net}} is a net translated from the EAL subset of BOHM that has undergone zero or more reduction steps.
\end{Definition}

\begin{Theorem}{Read back from a Valid EAL-Net gives is $α$ equivalent to the normal evaluation of the original EAL-term}{readback}
  Let $A$ be the Valid EAL-Net.
  \\ \\
  By Definition \ref{De:Valid EAL-Net} this net originated from a valid EAL-AST which is a subset of BOHM.
  \\ \\
  Thus by Theorem \ref{Th:AST->Net->Eval} $A$ must only have one free port.
  \\ \\
  Furthermore this free port must be the root of the graph/old AST.
  \\ \\
  Now to show this evaluates to the same answer, we must reconstruct the ADT considering all cases.
  \\ \\
  For this we must keep two maps as we do this algorithm.
  \\ \\
  The first being the variable name from a specific lambda.
  \\ \\
  The second map is a map from the fan in number to the current status of our traversal of said fan-in.
  \\ \\
  We can now write a proof by induction, specifically, we shall consider three cases:

  1. $\lambda$ /\ $\mu$

  2. $FanIn$

  3. The rest.

  We will also consider the inductive case first for the following proof.
  \\ \\
  Let $rec$ be this recursive algorithm.
  \begin{tcolorbox}[title= Case 1 - rest, colframe=inner-box-1]
    The logic for all other cases is simple. Consider some node $P$ with ports $1 \cdots n$.
    \\ \\
    Let port n be the inhabitant of the BOHM ADT.
    \\ \\
    By induction we run this algorithm and ports $1 \cdots n-1$ are $α$ equivalent to normal evaluation.
    \\ \\
    Thus we can construct ADT $P$ by $P(rec(n_1) \cdots rec(n_{n-1}))$.
    \\ \\
    $\therefore$ $P$ is well formed and we get an $α$ equivalent answer.
  \end{tcolorbox}

  \begin{tcolorbox}[title= Case 2 - $\lambda$ /\ $\mu$, colframe=inner-box-1]
    The $\lambda$ /\ $\mu$ case is special in that in a Valid EAL-Net the node will be traversed via the primary port via the parent AST and then by the second auxiliary port.
    \\ \\
    The second auxiliary port refers to the variable binding.
    \\ \\
    When the algorithm traverse the $\lambda$ node by the primary port first (this must be the case), we add a new var into the first map.
    \\ \\
    Then we run the recursive algorithm on Auxiliary 1, this subterm will only access the second Auxiliary port of this $\lambda$ node.
    \\ \\
    If the term is unused, then we will connect an eraser node to the second Auxiliary port.
    \\ \\
    This thus preserves the number of free ports.
    \\ \\
    By induction the subterms on Auxiliary one are also well formed
    \\ \\
    $\therefore$ the Lambda term is $α$ equivalent to the normal evaluation answer.
  \end{tcolorbox}

  \begin{tcolorbox}[breakable, break at=8cm, title= Case 3 - FanIn, colframe=inner-box-1]
    FanIn's are another interesting case, for this case we allocate a map from each FanIn to the traversal status.
    \\ \\
    For this, we keep a first-in-last-out stack of the following types.
    \begin{verbatim}
     type Status = In FanPort | Complete FanPort
     type FanPort = Circle | Star
    \end{verbatim}
    Where circle is the first auxiliary port and star is the second.
    \\ \\
    The Status type deserves some explanations. So $In$ means that we have visited either circle or star of a FanIn node without visiting the corresponding port on the FanOut.
    \\ \\
    The $Complete$ case means that we have visited the corresponding FanOut port, and have thus completed the traversal of the specific node. Much like how a ) closes a (. This is also why it is first-in-last-out, as we must close the most recent $In$ (parenthesis) before we can close the previous one.
    \begin{figure}[H]
        \begin{tikzpicture}
          \inetcell[]{a}{$f_i$}[5]
          \draw (-0.29, 0.23) node {$\scalebox{1.9}{$\circ$}$};
          \draw ( 0.29, 0.29) node {$\scalebox{1.7}{$\star$}$};
          \cutWire{a.out}{b.north}{}{}
        \end{tikzpicture}
    \end{figure}


    For this analysis we must consider what port we can enter from, however first we should note that before entering the primary port of a FanIn, that we must have first traversed through either $\star$ or $\circ$ first.
    \\ \\
    This is the case because entering a FanIn in this way is known as a FanOut, and denotes the end of sharing.
    And since ending sharing before starting is not possible from a valid EAL-Net, these cases never happen.
    \begin{tcolorbox}[title= Enter from Prim, colframe=inner-box-2, breakable]
      As shown above, we must have already traversed a FanIn node, namely $\star$ or $\circ$.
      \\ \\
      This means the map from the FanIn number $i$ is not empty, and thus we have a history of completing nodes or being in nodes.
      \\ \\
      If we have an unfinished $In$, then we pick the most recent $In$ port left to traverse.
      This is forced as this node is considered a FanOut and thus closes sharing.
      \\ \\
      We then mark the node as complete. If we have completed a single node, then we simply choose to leave through the other port.
      \\ \\
      Since we are working over valid EAL-Net's which require no oracle, there is no ambiguity in this mechanism. Furthermore, this fact also excludes any other configurations from happening.
    \end{tcolorbox}
    \begin{tcolorbox}[title= Enter from Aux, colframe=inner-box-2, breakable]
      We entered the FanIn through an auxiliary port, so we mark in the second map that we are $In$ this port at FanIn node $i$.
      \\ \\
      We then leave through the principle port, denoting the beginning of sharing.
    \end{tcolorbox}
    These cases handle all possible configurations in which we can enter and the validity of the expressions are handled by induction.
  \end{tcolorbox}
  Now, the base case is quite simple, consider a node, say with zero parameters by itself.
  \\ \\
  This node is trivially readback as is, being $α$ equivalent to the evaluated term.
  \\ \\
  $\therefore$ by induction read-back from a valid EAL-Net will give us the same expression up to $α$ equivalence as evaluating the node via a more traditional evaluation methods
\end{Theorem}

## Evaluation strategies

### Simple

First version: order-independent sequential reduction.
- Reduce primary pairs until we run out of primary pairs.
- Track a set of outstanding primary pairs. We will have an initial set when we create the graph. Whenever we connect ports, if the two ports are primary, add the pair to the set. When we reduce a pair, remove it from the set. (I think this will go in `linkPorts` or `relink`, one of those)
- Pick primary pairs from the set in any order and reduce them.

(in practice this set can be a stack or something)
(but we should try randomly reducing one of the pairs just to test)

Second version: order-independent parallel reduction the simple way, where we just have some thread pool of constant `n` threads and have each reduce a primary pair at once and somehow track the set in a synchronised way. From this we should learn what kind of locks or lock-free data structures or concurrency primitives (e.g. atomics) we need and will work most efficiently for parallel reduction.

Third version: start putting metadata on nodes (e.g. associated with linear logic types where we have some idea that subgraphs can be computed in parallel) and use this metadata at runtime to allocate threads more efficiently, possibly prove non-existence of certain kinds of contention and avoid concurrency primitives (e.g. locks) etc.

### Lazy (Levy-optimal)

#### Algorithm

1. Lazy: stop when root node is a constructor = root node connected by principal port to operator
1. Each step:
    1. Look for leftmost-outermost redex: starting from root of term, traverse any operator reached by an auxiliary port by existing from its principal port until we reach an operator at a principal port
        1. Either the previous operator was the root node, in which case halt
        1. Or we have found a redex (principal pair) - fire the redex and start again from the root
    1. To avoid re-scanning the term from the root, push operators onto a stack while traversing
        1. After firing a redex, pop the last operator and restart search

#### Properties

1. Minimises number of beta-reductions
1. No parallelism

### Parallel

#### Algorithm

1. Evaluate all primary pairs each step
1. Initially scan for primary pairs, then only examine changed nodes each step

#### Properties

1. Maximally parallel
1. Scanning may be expensive
1. May reduce unnecessary redexes (compared to lazy algorithm)
1. Unlikely to make sense if possible parallelism exceeds number of available cores

### Hybridised

#### Algorithm

1. As lazy, but when parallel annotations are hit, fork process (or assign thread up to number of processors)

#### Properties

1. Requires parallel annotations placed by the compiler
    1. Can definitely be placed when $\parr$ types are used
    1. Use proofs about terms being used to annotate graph?

### Implementation concerns

#### Thread affinity & cache locality

1. Tie a thread to a spacial area of the interaction net.

#### Safety under concurrency

1. How to avoid locks in rewriting?
