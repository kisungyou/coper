#' (Sample) Covariance
#' 
#' Compute an empirical sample covariance.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a named list containing: \describe{
#' \item{S}{a \eqn{(p\times p)} covariance matrix.}
#' }
#' 
#' @examples 
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 50
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compute sample covariance
#' S = coper::cov(dat)$S
#' 
#' ## true model parameter from standard normal
#' I = diag(5)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2), pty="s")
#' image(I, xaxt='n', yaxt='n', main="model")
#' image(S, xaxt='n', yaxt='n', main="sample covariance")
#' par(opar)
#' 
#' @concept covariance
#' @export
cov <- function(X){
  #-----------------------------------------------------
  # PREP
  fname = "cov"
  check_matrix(fname, X)

  #-----------------------------------------------------
  # COMPUTE
  S = cpp_covSAM(X)
  
  #-----------------------------------------------------
  # RETURN
  output = list(S=S)
  return(output)
}