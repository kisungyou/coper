#' Pearson Correlation
#' 
#' Compute a Pearson's correlation matrix.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a \eqn{(p\times p)} correlation matrix.
#' 
#' @examples 
#' \donttest{
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 25
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compute Pearson's correlation
#' C = coper::corr(dat)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(pty="s")
#' image(C, main="Pearson's correlation")
#' par(opar)
#' }
#' 
#' @concept correlation
#' @export
corr <- function(X){
  # CHECK
  fname = "corr"
  check_matrix(fname, X)
  
  # COMPUTE
  return(cov2corr_routine(cpp_covSAM(X)))
}