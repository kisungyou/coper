## AUXILIARY FUNCTIONS
#   (1) check_matrix     : input is a regular data matrix
#   (2) check_symposdiag : symmetric + positive diagonal
#   (3) check_spd        : check a SPD + positive diagonal





# (3) check_spd -----------------------------------------------------------
#' @keywords internal
#' @noRd
check_spd <- function(fname, X){
  cond1 = is.matrix(X)
  cond2 = isSymmetric(X)
  cond3 = (min(base::eigen(X, only.values=TRUE)$values) > .Machine$double.eps)
  
  if (!(cond1&&cond2&&cond3)){
    stop(paste0("* ",fname," : an input is not symmetric positive-definite."))
  }
}

# (1) check_matrix --------------------------------------------------------
#' @keywords internal
#' @noRd
check_matrix <- function(fname, X){
  cond1 = is.matrix(X)
  cond2 = !any(is.na(X))
  cond3 = !any(is.infinite(X))
  
  if (!(cond1&&cond2&&cond3)){
    stop(paste0("* ",fname," : an input is not a valid matrix."))}
}

# (2) check_symposdiag ----------------------------------------------------
#' @keywords internal
#' @noRd
check_symposdiag <- function(fname, Sigma){
  cond1 = is.matrix(Sigma)
  cond2 = isSymmetric(Sigma)
  cond3 = all(diag(Sigma) > 0)
  
  if (!(cond1&&cond2&&cond3)){
    stop(paste0("* ",fname," : an input should be (1) symmetric and (2) of positive diagonal vales."))
  }
}
