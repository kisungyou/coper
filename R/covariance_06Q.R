#' Covariance Estimation via Nearest Positive-Definite Matrix Projection
#' 
#' Qi and Sun (2006) proposed an algorithm for computing the positive correlation matrix
#' with \emph{Positive Definiteness} and transforming it back in order to estimate covariance matrix.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a named list containing: \describe{
#' \item{S}{a \eqn{(p\times p)} covariance matrix.}
#' }
#' 
#' @examples 
#' ## generate a toy data from standard normal
#' n   = 10
#' p   = 20
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## compare against sample covariance & true model
#' out1 <- coper::cov(dat)$S
#' out2 <- coper::cov06Q(dat)$S
#' out3 <- diag(p)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,3), pty="s")
#' image(out1, xaxt='n', yaxt='n', main="sample covariance")
#' image(out2, xaxt='n', yaxt='n', main="06Q estimate")
#' image(out3, xaxt='n', yaxt='n', main="model")
#' par(opar)
#' 
#' @references
#' \insertRef{qi_quadratically_2006}{coper}
#' 
#' @concept covariance
#' @export
cov06Q <- function(X){
  #-----------------------------------------------------
  # PREP
  fname = "cov06Q"
  check_matrix(fname, X)
  n = base::nrow(X)
  p = base::ncol(X)
  
  #-----------------------------------------------------
  # COMPUTE
  # 1. covariance & correlation
  S     = coper::cov(X)$S 
  diagS = diag(S)
  C     = coper::cov2corr(S)
  
  # 2. nearest PD projection
  Cadj = as.matrix(Matrix::nearPD(C, corr=TRUE)$mat)
  
  # 3. scale back
  outS = diag(sqrt(diagS))%*%Cadj%*%diag(sqrt(diagS))
  
  #-----------------------------------------------------
  # RETURN
  return(list(S=outS))
}