% improveCSS.Rd
% inserts a "<style>" tag that improves CSS for R documentation that is 
% rendered as HTML.

\newcommand{\testA}{\ifelse{html}{\out{La la la.}}{}}
\newcommand{\testB}{\ifelse{html}{\out{<style>h3 </style>}}{}}
\newcommand{\testC}{\ifelse{html}{\out{<style>h3 { color: red; }</style>}}{}}
\newcommand{\testD}{\ifelse{html}{\out{<style>p, pre, table { margin-top: 1ex; }</style>}}{}}
\newcommand{\testE}{\ifelse{html}{\out{<style>p, pre, table { margin-top: 1ex; } p { max-width: 65ch; }</style>}}{}}
\newcommand{\improveCSS}{\ifelse{html}{\out{<style>h3 { margin-top: 4.5ex; margin-bottom: 0; } p, pre, table { margin-top: 1ex; } p { max-width: 65ch; }</style>}}{}}
