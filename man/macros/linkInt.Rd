% linkInt.Rd
%
% Makes it easier to render links to internal functions. Specifically, the 
% macro renders links in monospaced fonts (as with \code{} or <pre>), and it 
% includes the parentheses of function names in the links. That is, a function 
% link will automatically be to (say) "regTable()" instead of to "regTable".  
%   The macro should be used only for links to internal package functions.
\newcommand{\linkInt}{\code{\link[=#1]{#1()}}} 


