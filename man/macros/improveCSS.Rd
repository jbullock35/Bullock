% improveCSS.Rd
% inserts a "<style>" tag that improves CSS for R documentation that is 
% rendered as HTML.
%
% NOTE: this macro should not be used. Instead, add CSS changes to the bottom 
% of inst/html/R.css.  [2019 12 26]

\newcommand{\improveCSS}{\ifelse{html}{\out{<style>h3 { margin-top: 4.5ex; margin-bottom: 0; } p, pre, table { margin-top: 1ex; } p { max-width: 65ch; }</style>}}{}}
