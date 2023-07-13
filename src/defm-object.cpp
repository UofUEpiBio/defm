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
//' @examples
//' # Loading Valente's SNS data
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   Y = valentesnsList$Y,
//'   X = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Adding the intercept terms and a motif from tobacco to mj
//' term_defm_logit_intercept(mymodel)
//' term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//' 
//' # Initialize the model
//' init_defm(mymodel)
//' 
//' # Fitting the MLE
/// defm_mle(mymodel)
//' 
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
SEXP set_names(
  SEXP m,
  const std::vector< std::string > & ynames,
  const std::vector< std::string > & xnames
) {

  Rcpp::XPtr< DEFM > ptr(m);
  ptr->set_names(
    ynames,
    xnames
  );

  return m;

}

//' Access to the names of a model's datasets
//'
//' Retrieve the column names of the dependent variable (`y`) and independent
//' variable (`x`) of an object of class [DEFM].
//'
//' @param m An object of class [DEFM].
//' @name defm-names
//' @returns A character vector.
//' @export
//' @return A character vector with the names of the dependent or independent
//' variables.
//' @examples
//' #' Using Valente's SNS data
//' data(valentesnsList)
//' 
//' # Creating the DEFM object
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   X = valentesnsList$X,
//'   Y = valentesnsList$Y,
//'   order = 0
//' )
//' 
//' # Getting the names
//' get_X_names(mymodel)
//' get_Y_names(mymodel)
// [[Rcpp::export(rng = false)]]
CharacterVector get_Y_names(
    SEXP m
) {
  Rcpp::XPtr< DEFM > ptr(m);
  return wrap(ptr->get_Y_names());
}

//' @rdname defm-names
//' @export
// [[Rcpp::export(rng = false)]]
CharacterVector get_X_names(
    SEXP m
) {
  Rcpp::XPtr< DEFM > ptr(m);
  return wrap(ptr->get_X_names());
}


//' @rdname DEFM
//' @param m An object of class `DEFM`.
//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP init_defm(SEXP m)
{
  Rcpp::XPtr< DEFM > ptr(m);
  ptr->init();
  return m;
}


// [[Rcpp::export(invisible = true, rng = false, name = "print_defm_cpp")]]
SEXP print_defm(SEXP x)
{

  Rcpp::XPtr< DEFM > ptr(x);

  ptr->print();

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
//' @examples
//' # Loading Valtente's SNS data
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id    = valentesnsList$id,
//'   Y     = valentesnsList$Y,
//'   X     = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Adding the intercept terms and a motif from tobacco to mj
//' term_defm_logit_intercept(mymodel)
//' term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//'
//' # Computing the log-likelihood
//' loglike_defm(mymodel, par = c(-1, -1, -1, 2), as_log = TRUE)
// [[Rcpp::export(rng = false)]]
double loglike_defm(SEXP m, std::vector< double > par, bool as_log = true)
{

  Rcpp::XPtr< DEFM > ptr(m);

  double res = ptr->likelihood_total(par, as_log);

  if (std::isfinite(res))
    return res;
  else
    return R_NegInf;

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

  ptr->set_seed(seed);

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
        res(i, j) = *(Y + j * nrows + i); // Column wise
      else
        res(i, j) = out[iter]; // But the simulation is row wise

      iter++;

    }

  }

  return res;
}


//' @export
//' @rdname DEFM
//' @param i An integer scalar indicating which set of statistics to print (see details.)
//' @details
//' The `print_stats` function prints the supportset of the ith type
//' of array in the model.
// [[Rcpp::export(rng = false, invisible = true)]]
int print_stats(SEXP m, int i = 0)
{
  Rcpp::XPtr< DEFM > ptr(m);
  ptr->print_stats(static_cast< unsigned int >(i));

  return 0;
}

//' @export
//' @rdname DEFM
//' @returns - `nterms_defm` returns the number of terms in the model.
// [[Rcpp::export(rng = false)]]
int nterms_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);
  return ptr->nterms();
}

// [[Rcpp::export(rng = false, name = "names.DEFM")]]
CharacterVector names_defm(SEXP x)
{

  Rcpp::XPtr< DEFM > ptr(x);
  return wrap(ptr->colnames());
}

//' @export
//' @rdname DEFM
//' @returns - `nrow_defm` returns the number of rows in the model.
// [[Rcpp::export(rng = false)]]
int nrow_defm(SEXP m)
{
  Rcpp::XPtr< DEFM > ptr(m);
  return ptr->get_n_rows();
}

//' @export
//' @rdname DEFM
//' @returns - `ncol_defm_y` returns the number of output variables in 
//' the model.
// [[Rcpp::export(rng = false)]]
int ncol_defm_y(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_y();

}

//' @export
//' @rdname DEFM
//' @returns - `ncol_defm_x` returns the number of covariates in the model.
// [[Rcpp::export(rng = false)]]
int ncol_defm_x(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_covars();

}

//' @export
//' @rdname DEFM
//' @returns - `nobs_defm` returns the number of observations (events) in the
//' model.
// [[Rcpp::export(rng = false)]]
int nobs_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_n_obs();

}

//' @export
//' @rdname DEFM
//' @returns - `morder_defm` returns the order of the Markov process.
// [[Rcpp::export(rng = false)]]
int morder_defm(SEXP m)
{

  Rcpp::XPtr< DEFM > ptr(m);

  return ptr->get_m_order();

}

//' Get sufficient statistics counts
//' 
//' This function computes the individual counts of the sufficient statistics
//' included in the model. 
//' @param m An object of class [DEFM].
//' @export
//' @return A matrix with the counts of the sufficient statistics.
//' @examples
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   Y = valentesnsList$Y,
//'   X = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Adding the intercept terms and a motif from tobacco to mj
//' term_defm_logit_intercept(mymodel)
//' term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//' 
//' # Initialize the model
//' init_defm(mymodel)
//' 
//' # Get the counts
//' head(get_stats(mymodel))
// [[Rcpp::export(rng = false)]]
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
      n_obs_i = 0u;

    // Did we passed the Markov order?
    if (n_obs_i++ < m_ord)
    {
      std::fill(res.row(i).begin(), res.row(i).end(), NA_REAL);
      continue;
    }

    for (size_t j = 0u; j < ncols; ++j)
      res(i, j) = (*target)[i_effective][j];

    i_effective++;

  }

  // Setting the names
  Rcpp::CharacterVector cnames(0);
  for (auto & n : model.colnames())
    cnames.push_back(n);
  Rcpp::colnames(res) = cnames;

  return res;

}

// [[Rcpp::export(rng = false)]]
NumericMatrix motif_census_cpp(SEXP m, std::vector<size_t> locs)
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

//' @param i,j The row and column of the array to turn on for the log odds.
//' @param par The parameters of the model.
//' @param m An object of class [DEFM].
//' @return - `logodds` returns a numeric vector with the log-odds for each observation in the data.
//' @rdname defm_mle
//' @export
// [[Rcpp::export(rng = false)]]
NumericVector logodds(
    SEXP m,
    const std::vector<double> & par,
    int i,
    int j
) {


  Rcpp::XPtr< DEFM > ptr(m);

  return wrap(ptr->logodds(par, i, j));

}

// [[Rcpp::export(rng = false)]]
LogicalVector is_motif(SEXP m) {
  Rcpp::XPtr< DEFM > ptr(m);

  return wrap(ptr->is_motif());

}


