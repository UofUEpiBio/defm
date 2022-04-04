#include <Rcpp.h>
#include "barry.hpp"
#include "defm.hpp"

using namespace Rcpp;

//' New DEFM
//' @export
// [[Rcpp::export(rng = false)]]
SEXP new_defm(
    const std::vector< int > & id,
    const IntegerMatrix & Y,
    const NumericMatrix & X,
    int order
  ) {

  Rcpp::XPtr< DEFM > model(new DEFM(
    &(id[0u]),
    &(Y(0,0)),
    &(X(0,0)),
    id.size(),
    Y.ncol(),
    X.ncol(),
    order
  ), true);

  model.attr("class") = "DEFM";

  return model;

}

//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
int init_defm(SEXP m)
{
  Rcpp::XPtr< DEFM > ptr(m);
  ptr->init();
  return 0;
}


//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_ones(SEXP m, int idx = -1)
{
  Rcpp::XPtr< DEFM > ptr(m);

  defmcounters::counter_ones(ptr->get_model().get_counters(), idx);

  return 0;
}


//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_fe(SEXP m, int idx = -1, double k = 1.0)
{
  Rcpp::XPtr< DEFM > ptr(m);

  defmcounters::counter_fixed_effect(ptr->get_model().get_counters(), idx, k);

  return 0;
}

//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_transition(SEXP m, std::vector< size_t > coords, int covar_idx = -1)
{
  Rcpp::XPtr< DEFM > ptr(m);

  defmcounters::counter_transition(ptr->get_model().get_counters(), coords, covar_idx);

  return 0;
}


//' @export
// [[Rcpp::export(invisible = true, rng = false, name = "print.DEFM")]]
SEXP print_defm(SEXP x)
{
  Rcpp::XPtr< DEFM > ptr(x);
  ptr->get_model().print();
  return x;
}


//' @export
// [[Rcpp::export(rng = false)]]
double loglike_defm(SEXP m, std::vector< double > par, bool as_log = true)
{
  Rcpp::XPtr< DEFM > ptr(m);
  return ptr->get_model().likelihood_total(par, as_log);
}

//' @export
// [[Rcpp::export(rng = false)]]
IntegerMatrix sim_defm(SEXP m, std::vector< double > par)
{

  Rcpp::XPtr< DEFM > ptr(m);
  IntegerMatrix res(ptr->get_n_rows(), ptr->get_n_y());

  std::fill(res.begin(), res.end(), -1);

  ptr->simulate(par, &(res[0u]));

  return res;
}
