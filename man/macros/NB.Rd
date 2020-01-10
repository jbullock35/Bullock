% NB.Rd 
% Creates the \NB macro to insert a non-breaking space that will render 
% correctly in PDF and in Windows browsers. 


\newcommand{\latexNB}{\out{~}}
\newcommand{\NB}{\ifelse{html}{\out{&nbsp;}}{\latexNB}}
