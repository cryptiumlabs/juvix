\begin{center}
\begin{tikzpicture}[->, >=stealth, shorten >=1pt, auto, node distance = 4cm, semithick]
  \definecolor{blue}{RGB}{6,56,110}
  \tikzstyle{every state}=[fill=blue, draw=none, text=white]

  \node[state] (frontend)                               {Juvix / Frontend};
  \node[state] (core)     [below of = frontend]         {Juvix / Core};
  \node[state] (optcore)  [below of = core]             {Juvix / Core};
  \node[state] (deal)     [below left of = optcore]     {Juvix / DEAL};
  \node[state] (inet)     [below right of = deal]       {Juvix / INet};
  \node[state] (machine)  [below of = inet]             {Underlying Machine};

  \path (frontend)  edge node {\textit{DESUGAR}}          (core);
  \path (core)      edge node {\textit{OPTIMISE}}         (optcore);
  \path (optcore)   edge node {\textit{INFER}}            (deal);
  \path (deal)      edge node {\textit{ENCODE}}           (inet);
  \path (optcore)   edge node {\textit{BESPOKE ENCODING}} (inet);
  \path (inet)      edge node {\textit{ABSALG}}           (machine);
\end{tikzpicture}
\end{center}

\pagebreak
