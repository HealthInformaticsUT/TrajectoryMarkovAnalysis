// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// stochasticMatrix
NumericMatrix stochasticMatrix(int matrixSize, CharacterVector states, DataFrame discreteTrajectories);
RcppExport SEXP _TrajectoryMarkovAnalysis_stochasticMatrix(SEXP matrixSizeSEXP, SEXP statesSEXP, SEXP discreteTrajectoriesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type matrixSize(matrixSizeSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type states(statesSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type discreteTrajectories(discreteTrajectoriesSEXP);
    rcpp_result_gen = Rcpp::wrap(stochasticMatrix(matrixSize, states, discreteTrajectories));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TrajectoryMarkovAnalysis_stochasticMatrix", (DL_FUNC) &_TrajectoryMarkovAnalysis_stochasticMatrix, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_TrajectoryMarkovAnalysis(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
