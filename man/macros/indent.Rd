% indent.Rd 
% Creates the \indent macro to add an indentation at the start of a paragraph. 


\newcommand{\latexIndent}{\out{\hspace*{.3in}}}
\newcommand{\indent}{\ifelse{html}{\NB\NB\NB\NB\NB\NB\NB\NB}{\latexIndent}}
