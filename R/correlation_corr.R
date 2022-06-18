#' Pearson Correlation
#' 
#' Compute a Pearson's correlation matrix.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a named list containing: \describe{
#' \item{C}{a \eqn{(p\times p)} partial correlation matrix.}
#' }
#' 
#' @examples 
#' \donttest{
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 25
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compute Pearson's correlation
#' C = coper::corr(dat)$C
#' 
#' ## true model parameter from standard normal
#' I = diag(5)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2), pty="s")
#' image(I, xaxt='n', yaxt='n', main="model")
#' image(C, xaxt='n', yaxt='n', main="Pearson's correlation")
#' par(opar)
#' }
#' 
#' @concept correlation
#' @export
corr <- function(X){
  #-----------------------------------------------------
  # PREP
  fname = "corr"
  check_matrix(fname, X)
  
  #-----------------------------------------------------
  # COMPUTE
  C = cov2corr_routine(cpp_covSAM(X))
  
  #-----------------------------------------------------
  # RETURN
  return(list(C=C))
}