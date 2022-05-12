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
//' @export
//' @name DEFM
//' @aliases new_defm defm
// [[Rcpp::export(rng = false)]]
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


//' Model specification for DEFM
//'
//' @param m An object of class [DEFM].
//' @param idx Integer. When greater than -1, index (starting from 0) of a covariate
//' used to weight the term.
//' @export
//'
//' @returns Invisible 0.
//'
//' @name defm_terms
//' @aliases terms_defm
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_ones(SEXP m, int idx = -1, std::string vname = "")
{

  Rcpp::XPtr< DEFM > ptr(m);
  defmcounters::counter_ones(ptr->get_model().get_counters(), idx, vname);

  return 0;
}


//' @rdname defm_terms
//' @export
//' @param k Numeric scalar. Exponent used in the term.
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_fe(SEXP m, int idx = -1, double k = 1.0, std::string vname = "")
{

  Rcpp::XPtr< DEFM > ptr(m);
  defmcounters::counter_fixed_effect(ptr->get_model().get_counters(), idx, k, vname);

  return 0;
}

//' @param mat Integer matrix. The matrix specifies the type of motif to capture
//' (see details.)
//' @details
//' In `term_defm_transition`, users can specify a particular motif to model. Motifs
//' are represented by cells with values equal to 1, for example, the matrix:
//'
//' ```  y0 y1 y2
//' t0:   1 NA NA
//' t1:   1  1 NA
//' ```
//'
//' represents a transition `y0 -> (y1, y2)`. If 0 is a motif of interest, then
//' the matrix should include 0 to mark zero values.
//' @export
//' @rdname defm_terms
// [[Rcpp::export(invisible = true, rng = false)]]
int term_defm_transition(
    SEXP m,
    IntegerMatrix & mat,
    int covar_idx = -1,
    std::string vname = ""
)
{

  Rcpp::XPtr< DEFM > ptr(m);

  if (static_cast<size_t>(mat.nrow()) != (ptr->get_m_order() + 1u))
    stop("The number of rows in -mat- must be equal to the Markov order of the model + 1.");

  if (static_cast<size_t>(mat.ncol()) != ptr->get_n_y())
    stop("The number of columns in -mat- must be equal to the number of y-columns in the model.");

  // Converting coordinates
  std::vector< size_t > coords(0u);
  std::vector< bool > signs(0u);
  int loc = -1;
  for (int j = 0u; j < mat.ncol(); ++j)
    for (int i = 0u; i < mat.nrow(); ++i)
    {

      loc++;

      // Only 1 or -1 make something
      if (mat(i,j) == R_NaInt)
        continue;

      if ((mat(i,j) != 1) & (mat(i,j) != 0))
        stop("Valid values for -mat- are NA, 0, or 1");

      coords.push_back(static_cast< size_t >(loc));
      signs.push_back(
        mat(i,j) == 1 ? true : false
      );

    }

  defmcounters::counter_transition(
    ptr->get_model().get_counters(), coords, signs,
      ptr->get_m_order(), ptr->get_n_y(),
      covar_idx, vname
      );

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
// [[Rcpp::export(rng = false)]]
IntegerMatrix sim_defm(
    SEXP m,
    std::vector< double > par,
    bool fill_t0 = true
  )
{


  Rcpp::XPtr< DEFM > ptr(m);

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
      std::fill(res.row(i).begin(), res.row(i).end(), NA_REAL);
      n_obs_i = 1u;
      continue;
    }

    // Did we passed the Markov order?
    if (n_obs_i++ < m_ord)
    {
      std::fill(res.row(i).begin(), res.row(i).end(), NA_REAL);
      continue;
    }

    for (size_t j = 0u; j < ncols; ++j)
      res(i, j) = (*target)[i_effective][j];

    // Completed the current
    ++i_effective;

  }

  // Setting the names
  Rcpp::CharacterVector cnames(0);
  for (auto & n : model.colnames())
    cnames.push_back(n);
  Rcpp::colnames(res) = cnames;

  return res;

}
