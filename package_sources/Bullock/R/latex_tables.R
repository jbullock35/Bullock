latable <- function(tables, substrings.to.remove=NULL, rows.to.remove=NULL, npmakebox=TRUE) {
  # Take a list of tables.  Create a matrix of data to xtable into LaTeX.  
  # Be sure that the argument is a list.  If it isn't, this won't work.
  #
  # Syntax: latable(list(glm1, glm2, glm3, etc)).
  #
  # substrings.to.remove should be a string or a vector of string.  It will be  
  # removed from the rownames of the output table.  [2011 02 17]
  #
  # TODO: As of now, latable() produces buggy output if the regressions are 
  # done with ivreg(), 'Intercept' is omitted through row.names.remove, and 
  # there is only one row of coefficients to report.  Fix this.  [2012 02 09]
  #
  # TODO: See whether I can improve the output using writeLines().  [2012 05 17]
  #
  # TODO: See if I can build more of a LaTeX wrapper into the output.  Among 
  # other things, this would make it easier for me to use Sweave.  [2012 05 17] 
  # 
  # TODO: Improve this function by combining code from the "SET UP THE OUTPUT 
  # MATRIX" and "ADD OTHER INFORMATION FROM THE FIRST REGRESSION" sections.  
  # This entails adding the R2, RMSE, and N rows -- or the corresponding rows 
  # if I am working with GLMs -- before I populate "output" with any values.  
  # [2011 02 18]
  #
  # TODO: I have changed this function and I am sure that it doesn't now work 
  # with glms.  Fix this.  [2011 02 17]
    
  # require(stringr)
  if (class(tables)!='list' & length(tables)==1) { 
    # If there is only one element in "tables", assume that the user just 
    # forgot to make it a list.  [2012 02 08]
    tables <- list(tables)
  } 
  else if (class(tables)!='list' & length(tables)>1) { 
    stop('The "tables" argument must be a list.')
  }
  if (class(substrings.to.remove)!='list') { 
    substrings.to.remove <- as.list(substrings.to.remove) 
  }      
  if (class(rows.to.remove)!='list') { 
    rows.to.remove <- as.list(rows.to.remove) 
  }      
    
  # SET UP THE OUTPUT MATRIX  
  if (class(tables[[1]])[1]=="vglm") { output <- summary(tables[[1]])@coef3[,1:2] }
  else { 
    tmp      <- sapply(tables, function (x) { names(x$coefficients) } )
    row.vars <- unique(as.vector(unlist(tmp))) # names of predictors, including intercept
    output   <- matrix(nrow=length(row.vars), ncol=length(tables)*2, 
                       dimnames=list(row.vars, rep(c('Estimate', 'SE'), length(tables))))
  }

  # POPULATE THE OUTPUT MATRIX WITH COEFFICIENTS AND STANDARD ERRORS [2011 02 16]
  # reg.num is an integer just indicating the location of a particular  
  #   regression in coefs.and.SEs [2011 02 16]
  # col.in.output is the column number in the "output" matrix of the coefficient 
  #   estimates for a particular regression.  For example, if length(tables)==3, 
  #   the second regression has reg.num==2, and col.in.output==3.  [2011 02 16] 
  coefs.and.SEs <- lapply(tables, function (x) summary(x)$coefficients[, 1:2])  
  for (reg.num in 1:length(coefs.and.SEs)) {
    input <- coefs.and.SEs[[reg.num]]
    col.in.output <- (reg.num*2) - 1
    for (i in 1:nrow(input)) {
      output[rownames(input)[i], c(col.in.output, col.in.output+1)] <- input[i, ]
    }
  }
  
  # ADD OTHER INFORMATION FROM THE FIRST REGRESSION
  if (class(tables[[1]])[1]=="lm") {
    output <- rbind(output, NA, NA, NA)
    rownames(output)[(nrow(output)-2):nrow(output)] <- c('R^2', 'RMSE', 'N')
    for (reg.num in 1:length(tables)) {
      col.in.output <- (reg.num*2) - 1      
      output['R^2',   col.in.output] <- summary(tables[[reg.num]])$r.squared
      output['RMSE', col.in.output] <- summary(tables[[reg.num]])$sigma
      output['N', col.in.output]    <- length(tables[[reg.num]]$fitted.values)
    }
  }
  else if (class(tables[[1]])[1] == "ivreg") {
      output <- rbind(output, NA)
      rownames(output)[nrow(output)] <- 'N'
      for (reg.num in 1:length(tables)) {
          col.in.output <- (reg.num * 2) - 1
          output["N", col.in.output] <- tables[[reg.num]]$n
          if (tables[[reg.num]]$n != tables[[reg.num]]$nobs) {
            print(paste('Warning: for regression ', reg.num, ', N is ', tables[[reg.num]]$n, 'but the number of observations with non-zero weights, "nobs", is only', tables[[reg.num]]$nobs, '.', sep=''))
          }
      }
  }
  else if (class(tables[[1]])[1]%in%c('glm', 'polr', 'negbin')) {
    llh  <- c(tables[[1]]$deviance/-2, NA)
    if (class(tables[[1]])[1]%in%c('negbin', 'glm')) { 
      N <- c(length(tables[[1]]$fitted.values), NA) 
    }
    else { N <- c(tables[[1]]$n, NA) }
    output <- round(rbind(output, llh, N), 2)    
  }
  else if (class(tables[[1]])=='zeroinfl') {
    output <- output[ceiling(nrow(output)/2):nrow(output), ]
    llh    <- c(tables[[1]]$llh, NA)
    N      <- c(tables[[1]]$n,   NA)
    output <- round(rbind(output, llh, N), 2)
  }
  else if (class(tables[[1]])=='vglm') { # assuming that this is multinomial logistic regression
    n.categories      <- ncol(summary(tables[[1]])@y) - 1 # number of categories in response variable, minus 1
    output.new        <- matrix(NA, nrow=nrow(output)/n.categories, ncol=(2*n.categories))
    for (i in 1:n.categories) {
      output.new[, ((2*i)-1):(2*i)] <- output[seq(i, nrow(output), by=n.categories), ]
    }
    llh    <- c(round(logLik(tables[[1]])),   NA)
    N      <- c(nrow(summary(tables[[1]])@x), NA) # e.g., nrow(summary(z.out)@x)
    output <- round(rbind(output.new, llh, N), 2)
    rownames.tmp <- c(rownames(summary(tables[[1]])@coef3)[seq(1, nrow(summary(tables[[1]])@coef3), by=n.categories)], 'Log likelihood', 'N')
    for (i in 1:length(rownames.tmp)) { rownames.tmp[i] <- sub(':1', '', rownames.tmp[i]) }
    rownames(output) <- rownames.tmp   
  }

  # ADD INFORMATION FROM SUBSEQUENT REGRESSIONS
  # This code doesn't add coefficients or standard errors from subsequent 
  # regressions.  That has already been added above.  Rather, this code adds
  # information on log likelihood, etc.  [2012 02 08]
  if (length(tables)!=1 && !class(tables[[1]])[1] %in% c('lm', 'ivreg')) {
    for (i in tables[2:length(tables)]) {
      #tmp <- summary(i)$coefficients[,1:2]
      if (class(i)[1]%in%c('glm', 'polr', 'negbin')) {
        tmp <- round(rbind(c(i$deviance/-2, NA), c(i$n, NA)), 2)
      }
      else if (class(i)=='zeroinfl') {
        tmp <- round(rbind(c(i$llh, NA), c(i$n, NA)), 2)
      }
      output <- cbind(output, tmp)
    }
  }
  
  # REMOVE UNWANTED ROWS
  if (!is.null(rows.to.remove)) { 
    for (pat in rows.to.remove) {
      output <- output[!grepl(pat, rownames(output)), ]
    }
  }

  # ADJUST ROWNAMES  
  rownames(output) <- gsub('TRUE', '', rownames(output))
  rownames(output) <- gsub('\\(Intercept\\)', 'Intercept', rownames(output))
  rownames(output) <- gsub('^I\\((.*)\\)', '\\1', rownames(output))
  rownames(output) <- gsub('^RMSE$', 'Standard error of regression', rownames(output))
  rownames(output) <- gsub('^N$', 'Number of observations', rownames(output))
  rownames(output) <- gsub('(.*)(\\^\\d)$', '\\1$\\2$', rownames(output))
    # TODO: (Low priority.)  Figure out how to put dollar signs around the  
    # substituted expression.  I need them for LaTeX.  But if I include them  
    # here, print.table() screws up the spacing of the corresponding rows.  I 
    # tried to adjust the spacing of the rows below, but it still didn't get the 
    # ampersands right.  [2011 02 18]

    # rownames(output)[nrow(output)-2] <- '\\textit{R}{\\relsize{-1}\\BPChem{\\^2}}'
  if (!is.null(substrings.to.remove)) { 
    for (i in substrings.to.remove) {
      rownames(output) <- gsub(i, '', rownames(output))
    }
  }
  if (class(tables[[1]])[1]%in%c('glm', 'polr', 'vglm', 'zeroinfl')) { 
    rownames(output)[nrow(output)-1] <- 'Log likelihood' 
  }
  if (class(tables[[1]])[1]=="polr") {
    n.thresholds <- (length(summary(tables[[1]])$lev))-2
    tmp <- rownames(output)[(nrow(output)-2-n.thresholds):(nrow(output)-2)]
    tmp <- paste('$\\tau_{\\text{', tmp, '}}$', sep='')
    rownames(output)[(nrow(output)-2-n.thresholds):(nrow(output)-2)] <- tmp
  }
  
  # ADJUST FORMATTING OF NUMBERS
  output <- round(output, 2)
  output.rownames <- rownames(output)
  output <- apply(output, 2, function (x) gsub('^(\\d)$', '\\1.00', x))
    # Replace '0' with '0.00', '1' with '1.00', etc.  [2011 02 17]
    # This converts numeric columns to characters.  One consequence is that
    # some numbers lose a trailing zero, e.g., .10 becomes '.1'.  [2011 02 17] 
  output <- apply(output, 2, function (x) gsub('0\\.', '.', x))
  output <- apply(output, 2, function (x) gsub('(\\.\\d)$', '\\10', x))
    # Replace, e.g., '.1' with '.10' [2011 02 17]
  output <- apply(output, 2, function (x) { x[is.na(x)] <- ''; x } )
    
  # ADJUST COLUMN SPACING [2011 02 17]
  colwidths <- apply(output, 2, function (x) max(nchar(x)))
  output    <- apply(output, 2, function (x) str_pad(x, side='left', width=max(nchar(x))))
    # This is not a job for print.table(x, justify='right').  That causes the 
    # columns to be very wide.  I don't know why.  [2011 02 18]
  #output[nrow(output)-2, 1] <- gsub('^ ', '', output[nrow(output)-2, 1])
  rownames(output) <- output.rownames

  # GENERATE LATEX
  # Interleave columns of ampersands.  [2011 02 17]
  output.latex <- matrix(NA, nrow=nrow(output), ncol=2*ncol(output), 
                         dimnames=list(rownames(output), NULL))
  for (i in 1:ncol(output)) { output.latex[, (2*i)] <- output[, i] }
  for (i in 1:ncol(output.latex)) {
    if (i%%2==1) { output.latex[, i] <- '&' }
  }
  
  # Adjust spacing: print.table() adds a space to each cell in rows that have 
  # dollar signs in their rownames.  [2011 02 18]
  # output.latex[nrow(output.latex)-2,] <- gsub('^ ', '', output.latex[nrow(output.latex)-2,])
  
  # Add white space between some rows.  [2011 02 17]
  rownames.output.latex <- rownames(output.latex)
  if (class(tables[[1]])[1] != 'ivreg') {
    output.latex <- rbind(output.latex[1, ], NA, 
                          output.latex[2:(nrow(output.latex)-3), ], NA, 
                          output.latex[(nrow(output.latex)-2):nrow(output.latex), ], 
                          NA)
    rownames.output.latex <- gsub('(Intercept)', 'Intercept', rownames.output.latex)
    rownames(output.latex) <- c(rownames.output.latex[1], '\addlinespace[.15in]',
                                rownames.output.latex[2:(length(rownames.output.latex)-3)],
                                '\addlinespace[.15in]',
                                rownames.output.latex[(length(rownames.output.latex)-2):length(rownames.output.latex)], 
                                '\bottomrule')
  }
  else {
    output.latex <- rbind(output.latex[1, ], NA, 
                          output.latex[2:(nrow(output.latex)-1), ], NA, 
                          output.latex[nrow(output.latex), ], 
                          NA)
    rownames.output.latex <- gsub('(Intercept)', 'Intercept', rownames.output.latex)
    rownames(output.latex) <- c(rownames.output.latex[1], '\addlinespace[.15in]',
                                rownames.output.latex[2:(length(rownames.output.latex)-1)],
                                '\addlinespace[.15in]',
                                rownames.output.latex[length(rownames.output.latex)], 
                                '\bottomrule')    
  }
                          
  # Add "\tabularnewline" to the ends of the rows.  [2011 02 19]
  output.latex[, ncol(output.latex)] <- gsub('$', '\tabularnewline', 
                                             output.latex[, ncol(output.latex)])                                                                                    

  # Adjust the "Number of observations" entries to better align them with the 
  # OLS estimates above them in each column.  [2011 02 19]
  if (npmakebox) { 
    #nregs    <- ncol(output.latex)/4
    nobsrow  <- which(rownames(output.latex)=='Number of observations')
    nobscols <- seq(2, ncol(output.latex), by=4)
    n.row    <- output.latex[nobsrow, ]
    lastrow  <- output.latex[nrow(output.latex), ]
    n.matrix <- matrix(NA, nrow=length(nobscols)-1, ncol=ncol(output.latex))
    
    # Splice "n.matrix" (currently a matrix of NAs) into output.latex.  [2011 02 20]
    output.latex <- rbind(output.latex[1:(nrow(output.latex)-1), ], 
                          #rep(NA, ncol(output.latex)/2),
                          n.matrix, 
                          '\bottomrule'=lastrow)
    for (i in nobscols) {
      formatted.n <- gsub(' ', '', n.row[i])
      formatted.n <- paste('\npmakebox[11.22][r]{', formatted.n, '}', sep='')
      if (i==2) { 
        output.latex[nobsrow + (i-2)/4,] <- c('&', formatted.n, '&&',
                                              rep('', ncol(output.latex)-3))
      } else if (i==max(nobscols)) {
        output.latex[nobsrow + (i-2)/4,] <- c('',  formatted.n, '\tabularnewline', 
                                              rep('', ncol(output.latex)-3))
      } else {
        output.latex[nobsrow + (i-2)/4,] <- c('',  formatted.n, '&&',
                                              rep('', ncol(output.latex)-3))
      }
    }
  }
                          
  # PRINT THE TABLE  [2011 02 20]
  # If using the npmakebox formatting for the "Number of observations" entry, 
  # we print output.latex as two separate tables.  This prevents the first  
  # data column of the table from being very wide (because it would need to 
  # accomodate all of the npmakebox formatting).  [2011 02 20]
  colnames(output.latex) <- rep('', ncol(output.latex))
  if (npmakebox) {
    print.table(output.latex[1:(nobsrow-1), ])
    print.table(output.latex[nobsrow:nrow(output.latex), ])
  } else {
    print.table(output.latex)
  }
}


table.sep <- function(table, separator='&', sig.digits=2) {
  # This function just interleaves separator columns between the columsn of 
  # any table that is passed to it.  [2011 05 09]
  # 
  # Use print.table() to print the output without quotation marks.  
  # [2011 05 09]
  stopifnot(class(table)%in%c('table', 'matrix', 'data.frame'))
  # require(gdata)
  sep.mat <- matrix(separator, nrow=nrow(table), ncol=ncol(table))
  
  # ROUND IF NECESSARY
  if (is.numeric(sig.digits) & is.numeric(table[,1])) {
    table <- round(table, sig.digits)
  }
   
  # INSERT THE AMPERSANDS.  [2011 05 09]
  out <- t(gdata::interleave(t(sep.mat), t(table)))
    # out <- t(gdata::interleave(t(table), t(sep.mat)))[, -(ncol(sep.mat)*2)]
  
  # ADJUST FORMATTING OF NUMBERS [2011 05 10]
  out <- apply(out, 2, function (x) gsub('^(\\d+)$', '\\1.00', x))
    # Replace '0' with '0.00', '123' with '123.00', etc.  [2011 02 17]
    # This converts numeric columns to characters.  One consequence is that
    # some numbers lose a trailing zero, e.g., .10 becomes '.1'.  [2011 02 17]
  out <- apply(out, 2, function (x) gsub('^0\\.', '.', x))
  out <- apply(out, 2, function (x) gsub('(\\.\\d)$', '\\10', x))
    # Replace, e.g., '.1' with '.10' [2011 02 17]
  out <- apply(out, 2, function (x) { x[is.na(x)] <- ''; x } )

  # ADD "\tabularnewline" TO THE ENDS OF THE ROWS.  [2011 05 10]
  out[, ncol(out)] <- gsub('$', '\tabularnewline', out[, ncol(out)])                                                                                    

  
  # RESTORE THE NUMERIC COLUMNS TO NUMERIC CLASS SO round() WILL WORK ON THE  
  # OBJECTS RETURNED BY THIS FUNCTION [2011 05 09]
  # This function doesn't work at present.  [2011 05 09]
  if (0==1 & class(table[,1])=='numeric') {
    out <- data.frame(out)
    for (i in 1:ncol(out)) {
      if (i%%2==1) { # if column is odd-numbered 
        out[,i] <- as.numeric(levels(out[,i]))
      }
    }
  }
  
  return(out)
}

####
#    tables <- list(lm1.Dem, lm3.Dem)
#    tmp      <- sapply(tables, function (x) { names(x$coefficients) })
#    row.vars <- unique(unlist(tmp)) # names of predictors, including intercept
#    output <- matrix(nrow=length(row.vars), ncol=length(tmp)*2, 
#                     dimnames=list(row.vars, rep(c('Estimate', 'SE'), length(tmp))))
#    coefs.and.SEs <- sapply(tables, function (x) summary(x)$coefficients[, 1:2])
#    for (reg.num in 1:length(coefs.and.SEs)) {
#      input <- coefs.and.SEs[[reg.num]]
#      col.in.output <- (reg.num*2) - 1 # column number of coefs. for the regression
#      for (i in 1:nrow(input)) {
#        output[rownames(input)[i], c(col.in.output, col.in.output+1)] <- input[i, ]
#      }
#    }              