- Desiderata
- Lambda cube
- Typing rules
- Examples

Define terms $t ::= T\ |\ P\ |\ x\ |\ t\ t\ |\ λx:e.e\ |\ ∀x:e.e$. Let $K = T\ |\ P$. Let $M, N$ be terms.

\begin{figure}[H]
\caption{Typing rules for the calculus of constructions}

\begin{prooftree}
\AxiomC{}
\RightLabel{}
\UnaryInfC{$Γ ⊢ P : T$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ A : K$}
\RightLabel{var}
\UnaryInfC{$Γ, x : A ⊢ x : A$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ, x : A ⊢ B : K$}
\AxiomC{$Γ, x : A ⊢ N : B$}
\RightLabel{lam}
\BinaryInfC{$Γ ⊢ (λx:A.N) : (∀x:A.B) : K$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ M : (∀x:A.B)$}
\AxiomC{$Γ ⊢ N : A$}
\RightLabel{app}
\BinaryInfC{$Γ ⊢ M N : B[x := N]$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ M : A$}
\AxiomC{$A =_β B$}
\AxiomC{$B : K$}
\RightLabel{conv}
\TrinaryInfC{$Γ ⊢ M : B$}
\end{prooftree}

\end{figure}
