#' Partial Correlation
#' 
#' Compute a partial correlation matrix using the regression method.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a named list containing: \describe{
#' \item{C}{a \eqn{(p\times p)} partial correlation matrix.}
#' }
#' 
#' @examples 
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 25
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compute partial correlation
#' C = coper::corr(dat)$C
#' 
#' ## true model parameter from standard normal
#' I = diag(5)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2), pty="s")
#' image(I, xaxt='n', yaxt='n', main="model")
#' image(C, xaxt='n', yaxt='n', main="partial correlation")
#' par(opar)
#' 
#' @concept correlation
#' @export
pcor <- function(X){
  #-----------------------------------------------------
  # PREP
  fname = "pcor"
  check_matrix(fname, X)
  p = base::ncol(X)

  #-----------------------------------------------------
  # COMPUTE
  output = array(1, c(p,p))
  for (i in 1:(p-1)){
    vecX = as.vector(X[,i])
    for (j in (i+1):p){
      vecY = as.vector(X[,j])
      matZ = X[,-c(i,j)]
      
      # regression : this means that there are rooms for generalized ones
      reg1 = as.vector(stats::lm(vecX~matZ)$residuals) 
      reg2 = as.vector(stats::lm(vecY~matZ)$residuals)
      
      output[i,j] = stats::cor(reg1, reg2)
      output[j,i] = output[i,j]
    }
  }

  #-----------------------------------------------------
  # PREP
  return(list(C=output))
}