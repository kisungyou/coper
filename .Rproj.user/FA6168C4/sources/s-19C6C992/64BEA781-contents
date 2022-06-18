#' Convert Covariance to Correlation
#' 
#' Given a symmetric matrix of positive diagonal entries, this function scales the 
#' matrix so that it has unit diagonals.
#' 
#' @param Sigma an \eqn{(p\times p)} symmetric matrix of positive diagonal entries.
#' 
#' @return a \eqn{(p\times p)} symmetric matrix of unit diagonals
#' 
#' @examples 
#' \donttest{
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 25
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compute sample covariance
#' S = coper::cov(dat)
#' 
#' ## convert into correlation
#' C = coper::cov2corr(S)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE) 
#' par(mfrow=c(1,2), pty="s")
#' image(S, main="covariance")
#' image(C, main="correlation")
#' par(opar)
#' }
#' 
#' @concept utility
#' @export
cov2corr <- function(Sigma){
  # CHECK
  fname = "cov2corr"
  check_symposdiag(fname, Sigma)
  
  # COMPUTE
  return(cov2corr_routine(Sigma))
}

#' @keywords internal
#' @noRd
cov2corr_routine <- function(Sigma){
  dvecinvhalf = diag(1/(sqrt(diag(Sigma))))
  output = dvecinvhalf%*%Sigma%*%dvecinvhalf
  return(output)  
}