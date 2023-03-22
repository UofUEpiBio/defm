// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// new_defm
SEXP new_defm(const SEXP& id, const SEXP& Y, const SEXP& X, int order);
RcppExport SEXP _defm_new_defm(SEXP idSEXP, SEXP YSEXP, SEXP XSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const SEXP& >::type id(idSEXP);
    Rcpp::traits::input_parameter< const SEXP& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const SEXP& >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(new_defm(id, Y, X, order));
    return rcpp_result_gen;
END_RCPP
}
// set_names
SEXP set_names(SEXP m, const std::vector< std::string >& ynames, const std::vector< std::string >& xnames);
RcppExport SEXP _defm_set_names(SEXP mSEXP, SEXP ynamesSEXP, SEXP xnamesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< const std::vector< std::string >& >::type ynames(ynamesSEXP);
    Rcpp::traits::input_parameter< const std::vector< std::string >& >::type xnames(xnamesSEXP);
    rcpp_result_gen = Rcpp::wrap(set_names(m, ynames, xnames));
    return rcpp_result_gen;
END_RCPP
}
// get_Y_names
CharacterVector get_Y_names(SEXP m);
RcppExport SEXP _defm_get_Y_names(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(get_Y_names(m));
    return rcpp_result_gen;
END_RCPP
}
// get_X_names
CharacterVector get_X_names(SEXP m);
RcppExport SEXP _defm_get_X_names(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(get_X_names(m));
    return rcpp_result_gen;
END_RCPP
}
// init_defm
SEXP init_defm(SEXP m);
RcppExport SEXP _defm_init_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(init_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// print_defm
SEXP print_defm(SEXP x);
RcppExport SEXP _defm_print_defm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(print_defm(x));
    return rcpp_result_gen;
END_RCPP
}
// loglike_defm
double loglike_defm(SEXP m, std::vector< double > par, bool as_log);
RcppExport SEXP _defm_loglike_defm(SEXP mSEXP, SEXP parSEXP, SEXP as_logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::vector< double > >::type par(parSEXP);
    Rcpp::traits::input_parameter< bool >::type as_log(as_logSEXP);
    rcpp_result_gen = Rcpp::wrap(loglike_defm(m, par, as_log));
    return rcpp_result_gen;
END_RCPP
}
// sim_defm
IntegerMatrix sim_defm(SEXP m, std::vector< double > par, bool fill_t0);
RcppExport SEXP _defm_sim_defm(SEXP mSEXP, SEXP parSEXP, SEXP fill_t0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::vector< double > >::type par(parSEXP);
    Rcpp::traits::input_parameter< bool >::type fill_t0(fill_t0SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_defm(m, par, fill_t0));
    return rcpp_result_gen;
END_RCPP
}
// print_stats
int print_stats(SEXP m, int i);
RcppExport SEXP _defm_print_stats(SEXP mSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(print_stats(m, i));
    return rcpp_result_gen;
END_RCPP
}
// nterms_defm
int nterms_defm(SEXP m);
RcppExport SEXP _defm_nterms_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(nterms_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// names_defm
CharacterVector names_defm(SEXP m);
RcppExport SEXP _defm_names_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(names_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// nrow_defm
int nrow_defm(SEXP m);
RcppExport SEXP _defm_nrow_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(nrow_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// ncol_defm_y
int ncol_defm_y(SEXP m);
RcppExport SEXP _defm_ncol_defm_y(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(ncol_defm_y(m));
    return rcpp_result_gen;
END_RCPP
}
// ncol_defm_x
int ncol_defm_x(SEXP m);
RcppExport SEXP _defm_ncol_defm_x(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(ncol_defm_x(m));
    return rcpp_result_gen;
END_RCPP
}
// nobs_defm
int nobs_defm(SEXP m);
RcppExport SEXP _defm_nobs_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(nobs_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// morder_defm
int morder_defm(SEXP m);
RcppExport SEXP _defm_morder_defm(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(morder_defm(m));
    return rcpp_result_gen;
END_RCPP
}
// get_stats
NumericMatrix get_stats(SEXP m);
RcppExport SEXP _defm_get_stats(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(get_stats(m));
    return rcpp_result_gen;
END_RCPP
}
// motif_census_cpp
NumericMatrix motif_census_cpp(SEXP m, std::vector<size_t> locs);
RcppExport SEXP _defm_motif_census_cpp(SEXP mSEXP, SEXP locsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::vector<size_t> >::type locs(locsSEXP);
    rcpp_result_gen = Rcpp::wrap(motif_census_cpp(m, locs));
    return rcpp_result_gen;
END_RCPP
}
// logodds
NumericVector logodds(SEXP m, const std::vector<double>& par, int i, int j);
RcppExport SEXP _defm_logodds(SEXP mSEXP, SEXP parSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< const std::vector<double>& >::type par(parSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(logodds(m, par, i, j));
    return rcpp_result_gen;
END_RCPP
}
// is_motif
LogicalVector is_motif(SEXP m);
RcppExport SEXP _defm_is_motif(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(is_motif(m));
    return rcpp_result_gen;
END_RCPP
}
// term_defm_ones
SEXP term_defm_ones(SEXP m, std::string idx, std::string vname);
RcppExport SEXP _defm_term_defm_ones(SEXP mSEXP, SEXP idxSEXP, SEXP vnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< std::string >::type vname(vnameSEXP);
    rcpp_result_gen = Rcpp::wrap(term_defm_ones(m, idx, vname));
    return rcpp_result_gen;
END_RCPP
}
// term_defm_fe
SEXP term_defm_fe(SEXP m, std::string idx, double k, std::string vname);
RcppExport SEXP _defm_term_defm_fe(SEXP mSEXP, SEXP idxSEXP, SEXP kSEXP, SEXP vnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< std::string >::type vname(vnameSEXP);
    rcpp_result_gen = Rcpp::wrap(term_defm_fe(m, idx, k, vname));
    return rcpp_result_gen;
END_RCPP
}
// term_defm_transition
SEXP term_defm_transition(SEXP m, IntegerMatrix& mat, std::string idx, std::string vname);
RcppExport SEXP _defm_term_defm_transition(SEXP mSEXP, SEXP matSEXP, SEXP idxSEXP, SEXP vnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< std::string >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< std::string >::type vname(vnameSEXP);
    rcpp_result_gen = Rcpp::wrap(term_defm_transition(m, mat, idx, vname));
    return rcpp_result_gen;
END_RCPP
}
// term_defm_transition_formula
SEXP term_defm_transition_formula(SEXP m, std::string formula, std::string idx, std::string vname);
RcppExport SEXP _defm_term_defm_transition_formula(SEXP mSEXP, SEXP formulaSEXP, SEXP idxSEXP, SEXP vnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type formula(formulaSEXP);
    Rcpp::traits::input_parameter< std::string >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< std::string >::type vname(vnameSEXP);
    rcpp_result_gen = Rcpp::wrap(term_defm_transition_formula(m, formula, idx, vname));
    return rcpp_result_gen;
END_RCPP
}
// term_defm_logit_intercept
SEXP term_defm_logit_intercept(SEXP m, IntegerVector coords, std::string idx, std::string vname);
RcppExport SEXP _defm_term_defm_logit_intercept(SEXP mSEXP, SEXP coordsSEXP, SEXP idxSEXP, SEXP vnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< std::string >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< std::string >::type vname(vnameSEXP);
    rcpp_result_gen = Rcpp::wrap(term_defm_logit_intercept(m, coords, idx, vname));
    return rcpp_result_gen;
END_RCPP
}
// rule_not_one_to_zero
SEXP rule_not_one_to_zero(SEXP m, std::vector< size_t > idx);
RcppExport SEXP _defm_rule_not_one_to_zero(SEXP mSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::vector< size_t > >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(rule_not_one_to_zero(m, idx));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_defm_new_defm", (DL_FUNC) &_defm_new_defm, 4},
    {"_defm_set_names", (DL_FUNC) &_defm_set_names, 3},
    {"_defm_get_Y_names", (DL_FUNC) &_defm_get_Y_names, 1},
    {"_defm_get_X_names", (DL_FUNC) &_defm_get_X_names, 1},
    {"_defm_init_defm", (DL_FUNC) &_defm_init_defm, 1},
    {"_defm_print_defm", (DL_FUNC) &_defm_print_defm, 1},
    {"_defm_loglike_defm", (DL_FUNC) &_defm_loglike_defm, 3},
    {"_defm_sim_defm", (DL_FUNC) &_defm_sim_defm, 3},
    {"_defm_print_stats", (DL_FUNC) &_defm_print_stats, 2},
    {"_defm_nterms_defm", (DL_FUNC) &_defm_nterms_defm, 1},
    {"_defm_names_defm", (DL_FUNC) &_defm_names_defm, 1},
    {"_defm_nrow_defm", (DL_FUNC) &_defm_nrow_defm, 1},
    {"_defm_ncol_defm_y", (DL_FUNC) &_defm_ncol_defm_y, 1},
    {"_defm_ncol_defm_x", (DL_FUNC) &_defm_ncol_defm_x, 1},
    {"_defm_nobs_defm", (DL_FUNC) &_defm_nobs_defm, 1},
    {"_defm_morder_defm", (DL_FUNC) &_defm_morder_defm, 1},
    {"_defm_get_stats", (DL_FUNC) &_defm_get_stats, 1},
    {"_defm_motif_census_cpp", (DL_FUNC) &_defm_motif_census_cpp, 2},
    {"_defm_logodds", (DL_FUNC) &_defm_logodds, 4},
    {"_defm_is_motif", (DL_FUNC) &_defm_is_motif, 1},
    {"_defm_term_defm_ones", (DL_FUNC) &_defm_term_defm_ones, 3},
    {"_defm_term_defm_fe", (DL_FUNC) &_defm_term_defm_fe, 4},
    {"_defm_term_defm_transition", (DL_FUNC) &_defm_term_defm_transition, 4},
    {"_defm_term_defm_transition_formula", (DL_FUNC) &_defm_term_defm_transition_formula, 4},
    {"_defm_term_defm_logit_intercept", (DL_FUNC) &_defm_term_defm_logit_intercept, 4},
    {"_defm_rule_not_one_to_zero", (DL_FUNC) &_defm_rule_not_one_to_zero, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_defm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
