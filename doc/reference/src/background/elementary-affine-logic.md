\begin{figure}[H]
\caption{Typing rules for elementary affine logic}

\begin{prooftree}
\AxiomC{}
\RightLabel{var}
\UnaryInfC{$A ⊢ A$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ B$}
\RightLabel{weak}
\UnaryInfC{$Γ, A ⊢ B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ_1 ⊢ A ⊸ B$}
\AxiomC{$Γ_2 ⊢ A$}
\RightLabel{app}
\BinaryInfC{$Γ_1,Γ_2 ⊢ B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ, A ⊢ B$}
\RightLabel{abst}
\UnaryInfC{$Γ ⊢ A ⊸ B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ_1 ⊢ \ !A_1, ..., Γ_n ⊢ \ !A_n$}
\AxiomC{$A_1, ..., A_n ⊢ B$}
\RightLabel{prom}
\BinaryInfC{$Γ_1, ..., Γ_n ⊢ \ !B$}
\end{prooftree}

\begin{prooftree}
\AxiomC{$Γ ⊢ !A$}
\AxiomC{$\ !A, ..., \ !A, ∆ ⊢ B$}
\RightLabel{contr}
\BinaryInfC{$Γ, ∆ ⊢ B$}
\end{prooftree}

\end{figure}
