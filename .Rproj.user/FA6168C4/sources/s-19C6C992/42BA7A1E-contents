/*
 * CPP Routines for Utility
 * (1) cpp_cov2pcor  : given a covariance matrix, compute a partial correlation
 */

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

// (1) cpp_cov2pcor
// [[Rcpp::export]]
arma::mat cpp_cov2pcor(arma::mat &Sigma){
  // preliminary
  int p = Sigma.n_rows;
  
  // inversion
  arma::mat Sinv = arma::inv_sympd(Sigma);
  
  // fill in 
  arma::mat output(p,p,arma::fill::ones);
  for (int i=0; i<(p-1); i++){
    for (int j=(i+1); j<p; j++){
      output(i,j) = -Sinv(i,j)/std::sqrt(Sinv(i,i)*Sinv(j,j));
      output(j,i) = output(i,j);
    }
  }
  return(output);
}
