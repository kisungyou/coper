// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_covSAM
arma::mat cpp_covSAM(arma::mat& X);
RcppExport SEXP _coper_cpp_covSAM(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_covSAM(X));
    return rcpp_result_gen;
END_RCPP
}
// cpp_cov2pcor
arma::mat cpp_cov2pcor(arma::mat& Sigma);
RcppExport SEXP _coper_cpp_cov2pcor(SEXP SigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type Sigma(SigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_cov2pcor(Sigma));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coper_cpp_covSAM", (DL_FUNC) &_coper_cpp_covSAM, 1},
    {"_coper_cpp_cov2pcor", (DL_FUNC) &_coper_cpp_cov2pcor, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_coper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}