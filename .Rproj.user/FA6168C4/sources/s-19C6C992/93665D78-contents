#' Partial correlation given a covariance matrix
#' 
#' If a covariance \eqn{\Sigma} is given, a partial correlation matrix is computed. 
#' Note that this function strictly requires a given input to be symmetric and 
#' positive-definite since it relies on matrix inversion.
#' 
#' 
#' 
#' 
#' @concept utility
#' @export
cov2pcor <- function(Sigma){
  # CHECK
  fname = "cov2pcor"
  check_spd(fname, Sigma)
  
  # COMPUTE
  return(cpp_cov2pcor(Sigma))
}