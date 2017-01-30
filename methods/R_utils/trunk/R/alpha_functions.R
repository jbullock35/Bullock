## There are three functions here.  alpha() computes alpha for a given matrix of data.
## reliability() gives alpha and the "alpha if deleted" lines for every item in a matrix of data.
## alpha.cronbach() SHOULD NOT BE USED by me; it is called by the reliability() function.

#alpha<-function(testdata){
#n<-ncol(testdata)
#nexmn<-nrow(testdata)
#x<-apply(testdata,1,sum)
#s2y<-diag(var(testdata))*(nexmn-1)/nexmn
#s2x<-var(x)*(nexmn-1)/nexmn
#alpha<-(n/(n-1))*(1-sum(s2y)/s2x)
#s2yy<-(((var(testdata)-diag(diag(var(testdata))))*(nexmn-1)/nexmn))^2
#lambda2<-1-sum(s2y)/s2x+sqrt((n/(n-1))*sum(s2yy))/s2x
#return(alpha,lambda2)}


alpha_cronbach <- function(S)
{
# Joseph F Lucke's cronbach's alpha function:
# S is the covariance matrix of the individual measurements
 d <- dim(S)[1]
 (d/(d - 1)) * (1 - sum(diag(S))/sum(S))
}

reliability <- function(x, ...)
{
    # Peter Ellis' reliability function, using Lucke's cronbach's alpha
    # x is a matrix of variables
    j <- dim(x)[2]
    out <- numeric(j)
    for (i in 1:j){
        out[i] <- alpha_cronbach(var(x[ ,-i], ...))
    }
    list(alpha=alpha_cronbach(var(x, ...)), alpha.if.deleted=out)
} 