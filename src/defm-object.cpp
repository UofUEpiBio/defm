#include <Rcpp.h>
#include "barry.hpp"
#include "defm.hpp"

using namespace Rcpp;

//' Discrete Exponential Family Model (DEFM)
//'
//' Discrete Exponential Family Models (DEFMs) are models from the exponential
//' family that deal with discrete data. Here, we deal with binary arrays which
//' can be used to represent, among other things, networks and multinomial binary
//' Markov processes.
//'
//' @param id Integer vector of length `n`. Observation ids, for example, person id.
//' @param Y 0/1 matrix of responses of `n_y` columns and `n` rows.
//' @param X Numeric matrix of covariates of size `n_x` by `n`.
//' @param order Integer. Order of the markov process, by default, 1.
//'
//' @return An external pointer of class `DEFM.`
//'
//' @name DEFM
//' @aliases new_defm defm
// [[Rcpp::export(rng = false, name = 'new_defm_cpp')]]
SEXP new_defm(
    const SEXP & id,
    const SEXP & Y,
    const SEXP & X,
    int order = 1
  ) {

  int n_id = LENGTH(id);
  int n_y  = Rf_ncols(Y);
  int n_x  = Rf_ncols(X);

  if (n_id <= order)
    stop("The -order- cannot be greater than the number of observations.");

  if (n_id != Rf_nrows(Y))
    stop("The number of rows in Y does not match the length of id.");

  if (n_id != Rf_nrows(X))
    stop("The number of rows in X does not match the length of id.");

  Rcpp::XPtr< DEFM > model(new DEFM(
    &(INTEGER(id)[0u]),
    &(INTEGER(Y)[0u]),
    &(REAL(X)[0u]),
    static_cast< size_t >(n_id),
    static_cast< size_t >(n_y),
    static_cast< size_t >(n_x),
    order
  ), true);

  model.attr("class") = "DEFM";

  return model;

}

// [[Rcpp::export(invisible = true, rng = false)]]
int set_names(
  SEXP m,
  const std::vector< std::string > & ynames,
  const std::vector< std::string > & xnames
) {

  Rcpp::XPtr< DEFM > ptr(m);
  ptr->set_names(
    ynames,
    xnames
  );

  return 0;

}

//' @rdname DEFM
//' @param m An object of class `DEFM`.
//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
int init_defm(SEXP m)
{
  Rcpp::XPtr< DEFM > ptr(m);
  ptr->init();
  return 0;
}


//' @export
//' @rdname defm_terms
// [[Rcpp::export(invisible = true, rng = false, name = "print.DEFM")]]
SEXP print_defm(SEXP x)
{

  Rcpp::XPtr< DEFM > ptr(x);

  ptr->get_model().print();

  return x;

}


//' Log-Likelihood of DEFM
//'
//' @param m An object of class [DEFM]
//' @param par A vector of parameters of length `nterms_defm(m)`.
//' @param as_log Logical scalar. When `TRUE` (default) returns the log-likelihood,
//' otherwise it returns the likelihood.
//' @return
//' Numeric, the computed likelihood or log-likelihood of the model.
//' @export
//'
// [[Rcpp::export(rng = false)]]
double loglike_defm(SEXP m, std::vector< double > par, bool as_log = true)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_model().likelihood_total(par, as_log);

}

//' Simulate data using a DEFM
//'
//' @param m An object of class [DEFM]. The baseline model.
//' @param par Numeric vector of model parameters.
//' @param fill_t0 Logical scalar. When `TRUE` (default) will fill-in the baseline
//' value of each observation (i.e., the starting condition) (see details.)
//'
//' @details
//' Each observation in the simulation must have initial condition. In practice,
//' this means we start the markov process with a matrix of size
//' `morder_defm(m) x ncol_defm_y(m)`, i.e., order of the Markov process times
//' the number of output variables. when `fill_t0 = TRUE`, the function return
//' the rows corresponding to baseline states with the original value, otherwise
//' it replaces them with -1. This option is mostly for testing purposes.
//'
//' @returns An integer vector of size `nrows_defm(m) x ncol_defm_y(m)`.
//' @export
// [[Rcpp::export(rng = true)]]
IntegerMatrix sim_defm(
    SEXP m,
    std::vector< double > par,
    bool fill_t0 = true
  )
{

  unsigned int seed = static_cast<unsigned int>(
    R::unif_rand() * std::numeric_limits<unsigned int>::max()
  );


  Rcpp::XPtr< DEFM > ptr(m);

  ptr->get_model().set_seed(seed);

  size_t nrows = ptr->get_n_rows();
  size_t ncols = ptr->get_n_y();

  std::vector< int > out(nrows * ncols, -1);
  ptr->simulate(par, &(out[0u]));

  IntegerMatrix res(nrows, ncols);

  size_t iter = 0u;
  const int * Y = ptr->get_Y();
  for (size_t i = 0u; i < nrows; ++i)
  {

    for (size_t j = 0u; j < ncols; ++j)
    {

      if (fill_t0 & (out[iter] == -1))
        res(i, j) = *(Y + j * ncols + i);
      else
        res(i, j) = out[iter];

      iter++;

    }

  }

  return res;
}


//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false, invisible = true)]]
int print_stats(SEXP m, int i = 0)
{
  Rcpp::XPtr< DEFM > ptr(m);
  ptr->get_model().print_stats(static_cast< unsigned int >(i));

  return 0;
}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int nterms_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);
  return ptr->get_model().nterms();
}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false, name = "names.DEFM")]]
CharacterVector names_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);
  return wrap(ptr->get_model().colnames());
}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int nrow_defm(SEXP m)
{
  Rcpp::XPtr< DEFM > ptr(m);
  return ptr->get_n_rows();
}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int ncol_defm_y(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_y();

}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int ncol_defm_x(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_covars();

}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int nobs_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_obs();

}

//' @export
//' @rdname DEFM
// [[Rcpp::export(rng = false)]]
int morder_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_m_order();

}

//' Get sufficient statistics counts
//' @param m An object of class [DEFM].
//' @export
// [[Rcpp::export]]
NumericMatrix get_stats(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  auto model = ptr->get_model();

  // Getting sizes
  size_t nrows = ptr->get_n_rows();
  size_t ncols = model.nterms();
  size_t m_ord = ptr->get_m_order();

  const int * ID = ptr->get_ID();

  NumericMatrix res(nrows, ncols);
  auto target = model.get_stats_target();

  size_t i_effective = 0u;
  size_t n_obs_i = 0u;
  for (size_t i = 0u; i < nrows; ++i)
  {

    // Do we need to reset the counter?
    if ((i > 0) && (*(ID + i - 1u) != *(ID + i)))
    {
      n_obs_i = 1u;

      // Completed the current
      ++i_effective;
    }

    // Did we passed the Markov order?
    if (n_obs_i++ < m_ord)
    {
      std::fill(res.row(i).begin(), res.row(i).end(), NA_REAL);
      continue;
    }

    for (size_t j = 0u; j < ncols; ++j)
      res(i, j) = (*target)[i_effective][j];

  }

  // Setting the names
  Rcpp::CharacterVector cnames(0);
  for (auto & n : model.colnames())
    cnames.push_back(n);
  Rcpp::colnames(res) = cnames;

  return res;

}

//' Motif census
//' @param m An object of class [DEFM].
//' @param locs Idx (starting from zero) with the variables that will be
//' included in the census.
//' @export
// [[Rcpp::export]]
NumericMatrix motif_census(SEXP m, std::vector<size_t> locs)
{

  Rcpp::XPtr< DEFM > ptr(m);

  barry::FreqTable<int> res = ptr->motif_census(locs);
  auto dat = res.get_data();

  size_t nele = res.size();
  NumericMatrix m_res(nele, locs.size() * (ptr->get_m_order() + 1) + 1);

  size_t ele = 0u;
  for (size_t i = 0u; i < nele; ++i)
    for (size_t j = 0u; j < (locs.size() * (ptr->get_m_order() + 1) + 1); ++j)
      m_res(i, j) = dat[ele++];

  // Setting the names
  Rcpp::CharacterVector cnames = {"count"};
  for (size_t m = 0u; m < (ptr->get_m_order() + 1); ++m)
  {
    for (auto & n : locs)
      cnames.push_back(
        std::string("y") + std::to_string(m) + std::to_string(n)
      );
  }

  Rcpp::colnames(m_res) = cnames;

  return m_res;

}

//' Log odds (aka conditional prob, aka gibbs sampler)
//' @export
// [[Rcpp::export]]
NumericVector logodds(
    SEXP m,
    const std::vector<double> & par,
    int i,
    int j
) {


  Rcpp::XPtr< DEFM > ptr(m);

  return wrap(ptr->logodds(par, i, j));

}
