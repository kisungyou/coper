#' Rao-Blackwell Ledoit-Wolf Estimator
#' 
#' Authors propose to estimate covariance matrix by minimizing mean squared error with the following formula,
#' \deqn{\hat{\Sigma} = \rho \hat{F} + (1-\rho) \hat{S}}
#' where \eqn{\rho \in (0,1)} a control parameter/weight, \eqn{\hat{S}} an empirical covariance matrix, and \eqn{\hat{F}} a \emph{target} matrix.
#' It is proposed to use a structured estimate \eqn{\hat{F} = \textrm{Tr} (\hat{S}/p) \cdot I_{p\times p}} where \eqn{I_{p\times p}} is an identity matrix of size \eqn{p}.
#' 
#' @param X an \eqn{(n\times p)} matrix whose rows are observations.
#' 
#' @return a named list containing: \describe{
#' \item{S}{a \eqn{(p\times p)} covariance matrix.}
#' \item{rho}{an estimate for convex combination weight.}
#' }
#' 
#' @examples 
#' ## SIMPLE EXAMPLE ---------------------------------------
#' ## generate a toy data from 5-dimensional standard normal
#' n   = 25
#' p   = 5
#' dat = matrix(stats::rnorm(n*p), ncol=p)
#' 
#' ## estimate
#' Sraw = coper::cov(dat)$S
#' Snow = coper::covRBLW(dat)$S
#' 
#' ## true model parameter from standard normal
#' I = diag(5)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,3), pty="s")
#' image(I,    xaxt='n', yaxt='n', main="model")
#' image(Sraw, xaxt='n', yaxt='n', main="sample covariance")
#' image(Snow, xaxt='n', yaxt='n', main="RBLW estimate")
#' par(opar)
#' 
#' \dontrun{
#' ## EXPERIMENT -------------------------------------------
#' ## now, we want to see how 'rho' is determined contingent
#' ## on the number of observations available.
#' 
#' ## define a sequence : varying number of observations
#' vec_sam = seq(from=5, to=200, by=5)
#' 
#' ## we will record 'rho' and 'norm differences'
#' vec_rho  = rep(0, length(vec_sam))
#' vec_diff = rep(0, length(vec_sam))
#' 
#' ## iterate
#' for (i in 1:length(vec_sam)){
#'   # data generation & run
#'   dat_norun <- matrix(rnorm(5*vec_sam[i]), ncol=5)
#'   out_norun <- coper::covRBLW(dat_norun)
#'   
#'   # record
#'   vec_rho[i]  = out_norun$rho
#'   vec_diff[i] = norm(out_norun$S - diag(5), "f")
#' }
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#' plot(vec_sam, vec_rho, main="estimated rhos",
#'      lwd=2, type="b", col="red")
#' plot(vec_sam, vec_diff, main="Frobenius error",
#'      lwd=2, type="b", col="blue")
#' par(opar)
#' }
#' 
#' @references
#' \insertRef{chen_shrinkage_2010}{coper}
#' 
#' @concept covariance
#' @export
covRBLW <- function(X){
  #-----------------------------------------------------
  # PREP
  fname = "covRBLW"
  check_matrix(fname, X)
  n = base::nrow(X)
  p = base::ncol(X)
  
  #-----------------------------------------------------
  # COMPUTATION
  # 1. MLE and related values
  Shat = cpp_covSAM(X)*(n-1)/n
  trS  = aux_trace(Shat)
  trS2 = aux_trace(Shat%*%Shat)
  
  # 2. structured estimate
  Fhat = (trS/p)*diag(p)
  
  # 3. rho
  term1 = ((n-2)/n)*trS2 + (trS^2)
  term2 = (n+2)*(trS2 - ((trS^2)/p))
  rhohat = term1/term2
  rhohat = max(min(rhohat, 1),0) # adjust as in LW case
  
  #-----------------------------------------------------
  # RETURN
  output     = list()
  output$S   = (1-rhohat)*Shat + rhohat*Fhat
  output$rho = rhohat
  return(output)
}
  


# auxiliary functions -----------------------------------------------------
#' @keywords internal
#' @noRd
aux_trace <- function(X){
  return(sum(diag(X)))
}
