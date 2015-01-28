# qw.R
# created 2014 May 24

# Provides Perl-like qw() for easy quoting of many strings.  Created by 
# "flodel" at http://stackoverflow.com/questions/520810/.

qw <- function(x) {
  x <- sub('^\\s+', '', x)            # eliminate spaces at the beginning of x
  unlist(strsplit(x, "[[:space:]]+"))
}
