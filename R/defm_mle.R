#' Maximum Likelihood Estimation of DEFM
#'
#' Fits a Discrete Exponential-Family Model using Maximum Likelihood.
#'
#' @param object An object of class [DEFM].
#' @param start Double vector or named list. Starting point for the MLE
#' (defaults to zero). When named, values are matched to the model terms.
#' @param lower,upper Lower and upper limits for the optimization (only used
#' with `method = "L-BFGS-B"`). Default to -20 and 20.
#' @param method Character scalar. Either `"L-BFGS-B"` (default) or `"BFGS"`,
#' passed to [stats::optim()], or `"newton"` for Newton-Raphson with step
#' halving (see details).
#' @param fixed Named list or named numeric vector. Parameters held fixed at
#' the given values.
#' @param ncores Integer scalar. Number of threads used to compute the
#' log-likelihood and its derivatives (only relevant if the package was
#' compiled with OpenMP).
#' @param control List of control parameters. Passed to [stats::optim()];
#' with `method = "newton"`, `maxit` (default 100) and `reltol` (default
#' `1e-10`) are used.
#' @param ... Further arguments passed to [stats::optim()].
#' @export
#' @import stats4
#' @details 
#' The likelihood function of the DEFM is closely-related to the 
#' Exponential-Family Random Graph Model \[ERGM\]. Furthermore, the DEFM can 
#' be treated as a generalization of the ERGM. The model implemented here
#' can be viewed as an ERGM for a bipartite network, where the actors are
#' individuals and the events are the binary outputs.
#' 
#' If the model features no markov terms, i.e., terms that depend on more than
#' one output, then the model is equivalent to a logistic regression. The 
#' example below shows this equivalence.
#' 
#' The optimization uses the exact gradient of the log-likelihood
#' ([loglike_defm_grad()]), and the variance-covariance matrix is the inverse
#' of minus the exact Hessian ([loglike_defm_hess()]) at the estimates. Since
#' the log-likelihood is concave, `method = "newton"` usually converges in a
#' few iterations, but, unlike `"L-BFGS-B"`, it does not bound the
#' parameters, so it may drift when the MLE does not exist (e.g., under
#' separation).
#'  
#' @return An object of class `defm_mle`, which extends [stats4::mle]
#' (all the methods for `mle` objects, e.g., `coef()`, `vcov()`, and
#' `summary()`, work with it). It also stores the names of the outcomes and
#' covariates, which are used by [summary_table()] to build a compact table
#' of the estimates.
#' @examples
#' #' Using Valente's SNS data
#' data(valentesnsList)
#' 
#' # Creating the DEFM object
#' logit_0 <- new_defm(
#'   id = valentesnsList$id,
#'   X = valentesnsList$X,
#'   Y = valentesnsList$Y[,1,drop=FALSE],
#'   order = 0
#' )
#' 
#' # Building the model
#' td_logit_intercept(logit_0)
#' td_logit_intercept(logit_0, covar = "Hispanic")
#' td_logit_intercept(
#'   logit_0,
#'   covar = "exposure_smoke"
#' )
#' td_logit_intercept(logit_0, covar = "Grades")
#' init_defm(logit_0) # Needs to be initialized
#' 
#' # Fitting the model
#' res_0 <- defm_mle(logit_0)
#' 
#' # Refitting the model using GLM
#' res_glm <- with(
#'   valentesnsList,
#'   glm(Y[,1] ~ X[,1] + X[,3] + X[,7], family = binomial())
#'   )
#' 
#' # Comparing results
#' summary_table(res_0)
#' summary(res_0)
#' summary(res_glm)
#' 
#' # Comparing the logodds
#' head(logodds(logit_0, par = coef(res_0), i = 0, j = 0))
#' 
#' @seealso [DEFM] for objects of class DEFM and [loglike_defm()] for the
#' log-likelihood function of DEFMs.
#' @references 
#' Vega Yon, G. G., Pugh, M. J., & Valente, T. W. (2022). Discrete Exponential-Family Models for Multivariate Binary Outcomes (arXiv:2211.00627). arXiv. \url{https://arxiv.org/abs/2211.00627}
defm_mle <- function(
    object,
    start,
    lower,
    upper,
    method  = c("L-BFGS-B", "BFGS", "newton"),
    fixed   = list(),
    ncores  = 1L,
    control = list(),
    ...
) {

  if (!inherits(object, "DEFM"))
    stop("-object- must be an object of class \"DEFM\"")

  call   <- match.call()
  method <- match.arg(method)
  pnames <- names(object)
  K      <- length(pnames)

  # stats4::profile() passes -minuslogl- when refitting; it is not needed
  dots <- list(...)
  dots$minuslogl <- NULL

  # Parameter vectors (start, fixed, bounds) as full-length named vectors
  as_full <- function(x, default, what) {

    x <- unlist(x)
    v <- structure(rep(default, K), names = pnames)

    if (!length(x))
      return(v)

    if (is.null(names(x))) {
      v[] <- rep_len(as.numeric(x), K)
      return(v)
    }

    idx <- match(names(x), pnames)
    if (anyNA(idx))
      stop(
        "Some names in -", what, "- are not terms in the model: ",
        paste(names(x)[is.na(idx)], collapse = ", ")
      )

    v[idx] <- as.numeric(x)
    v

  }

  start   <- if (missing(start)) as_full(0, 0, "start") else
    as_full(start, 0, "start")
  fixed   <- as_full(fixed, NA_real_, "fixed")
  isfixed <- !is.na(fixed)
  start[isfixed] <- fixed[isfixed]
  free    <- which(!isfixed)

  full <- function(p) {
    v <- start
    v[free] <- p
    v
  }

  fn <- function(p)
    -loglike_defm(object, full(p), as_log = TRUE, ncores = ncores)

  gr <- function(p)
    -loglike_defm_grad(object, full(p), ncores = ncores)[free]

  hess <- function(p)
    -loglike_defm_hess(object, full(p), ncores = ncores)[
      free, free, drop = FALSE
    ]

  if (method == "newton") {

    oout <- defm_newton(start[free], fn, gr, hess, control)

  } else {

    args <- c(
      list(
        par     = start[free],
        fn      = fn,
        gr      = gr,
        method  = method,
        control = control
      ),
      dots
    )

    if (method == "L-BFGS-B") {

      lower <- if (missing(lower)) as_full(-20, -20, "lower") else
        as_full(lower, -Inf, "lower")
      upper <- if (missing(upper)) as_full(20, 20, "upper") else
        as_full(upper, Inf, "upper")

      if (any(fixed[isfixed] < lower[isfixed] | fixed[isfixed] > upper[isfixed]))
        stop("fixed values violate constraints")

      lower <- lower[free]
      upper <- upper[free]

      if (any(args$par < lower | args$par > upper)) {
        warning("start values do not satisfy constraints")
        args$par <- pmin(pmax(args$par, lower), upper)
      }

      args$lower <- lower
      args$upper <- upper

    }

    oout <- do.call(stats::optim, args)

  }

  # Exact Hessian of minus the log-likelihood at the estimates
  oout$hessian <- hess(oout$par)
  vcov <- tryCatch(
    solve(oout$hessian),
    error = function(e) {
      warning(
        "The Hessian is singular at the estimates; the variance-covariance ",
        "matrix is not available (", conditionMessage(e), ")."
      )
      matrix(NA_real_, length(free), length(free))
    }
  )

  coef <- structure(oout$par, names = pnames[free])
  dimnames(vcov) <- list(pnames[free], pnames[free])

  minuslog <- sprintf(
    "function(%s) {
      par <- c(%1$s)
      -loglike_defm(object, par, as_log = TRUE)
    }", paste0("`", pnames, "`", collapse = ", ")
  )

  minuslog <- gsub("\\\\", "\\\\\\", minuslog)

  minuslog <- eval(parse(text = minuslog))

  methods::new(
    "defm_mle",
    call      = call,
    coef      = coef,
    fullcoef  = full(oout$par),
    fixed     = fixed,
    vcov      = vcov,
    min       = oout$value,
    details   = oout,
    minuslogl = minuslog,
    nobs      = as.integer(nrow_defm(object) + ifelse(
      morder_defm(object) > 0, -nobs_defm(object), 0L
    )),
    method    = method,
    y_names   = as.character(get_Y_names(object)),
    x_names   = as.character(get_X_names(object))
  )

}

#' Newton-Raphson with step halving
#' @param par Starting values.
#' @param fn,gr,hess Objective (minus log-likelihood), its gradient, and its
#' Hessian.
#' @param control List with `maxit` and `reltol`.
#' @return A list with the same elements as [stats::optim()].
#' @noRd
defm_newton <- function(par, fn, gr, hess, control = list()) {

  maxit  <- if (is.null(control$maxit)) 100L else control$maxit
  reltol <- if (is.null(control$reltol)) 1e-10 else control$reltol

  value       <- fn(par)
  n_fn        <- 1L
  convergence <- 1L
  message     <- "iteration limit reached"

  for (iter in seq_len(maxit)) {

    g <- gr(par)
    step <- tryCatch(
      solve(hess(par), g),
      error = function(e) g # Falls back to steepest descent
    )

    # Halving the step until the objective does not increase
    t <- 1
    repeat {

      candidate <- par - t * step
      value_new <- fn(candidate)
      n_fn <- n_fn + 1L

      if (is.finite(value_new) && (value_new <= value))
        break

      t <- t / 2
      if (t < 1e-10)
        break

    }

    if (t < 1e-10) {
      convergence <- 52L
      message     <- "step halving failed to decrease the objective"
      break
    }

    change <- abs(value - value_new)
    par    <- candidate
    value  <- value_new

    if (change <= reltol * (abs(value) + reltol)) {
      convergence <- 0L
      message     <- NULL
      break
    }

  }

  list(
    par         = par,
    value       = value,
    counts      = c("function" = n_fn, gradient = iter),
    convergence = convergence,
    message     = message
  )

}

#' @importFrom stats pnorm
pval_calc <- function(obj) {
  stats::pnorm(
    -abs(stats4::coef(obj)/sqrt(diag(stats4::vcov(obj))))
    ) * 2
}

#' Takes a list of a single fitted model and groups the terms into
#' interpretable groups.
#' @param fits A fitted model or a list of fitted models.
#' @param skip_intercept TRUE/FALSE. Whether to skip the intercept terms
#' from the output.
#' @return A list with three elements:
#' * `custom.coef.map`: A named list that maps the original coefficient names
#' to a cleaner version.
#' * `reorder.coef`: An integer vector that indicates how to reorder the
#' coefficients.
#' * `groups`: A named list that groups the coefficients into interpretable
#' groups.
#' @noRd
group_terms <- function(
  fits,
  skip_intercept = FALSE
  ) {

  # We want to treat all as a list
  if (!inherits(fits, "list")) {
    fits <- list(fits)
  }

  replacement <- c(
    "Logit intercept\\s+(.+)" = "\\1",
    "(Motif\\s+)?\\{([^,]+),\\s*([^}]+)\\}" = "{\\2, \\3}",
    "(Motif\\s+)?\\{([^,]+),\\s*([^}]+)\\}(.+)" = "{\\2, \\3}\\4"
  )

  vnames <- lapply(fits, coef) |>
    unname() |>
    unlist() |>
    names() |>
    unique()

  vnames_new <- gsub(
    x = vnames,
    replacement = replacement[1],
    pattern = names(replacement)[1]
  ) |>
    gsub(
      replacement = replacement[2],
      pattern = names(replacement)[2]
    )

  # Identifying the order
  term_patterns <- names(replacement)
  terms_intercept <- which(grepl(pattern = term_patterns[1], x = vnames))
  terms_motifs <- which(grepl(pattern = term_patterns[2], x = vnames))
  terms_transitions <- which(grepl(pattern = term_patterns[3], x = vnames))

  # Removing the motifs from the transitions
  terms_motifs <- setdiff(terms_motifs, terms_transitions)

  # Reorganizing terms in this sort
  terms_order <- c(terms_intercept, terms_motifs, terms_transitions)

  terms_groups <- NULL
  if (length(terms_intercept) > 0) {
    terms_groups <- c(terms_groups, list(
      "Logit intercept" = seq_along(terms_intercept)
    ))
  }

  nterms <- length(unlist(terms_groups))
  if (length(terms_motifs) > 0) {
    terms_groups <- c(terms_groups, list(
      "Co-ocurrence" = seq_along(terms_motifs) + nterms
    ))
  }

  nterms <- length(unlist(terms_groups))
  if (length(terms_transitions) > 0) {
    terms_groups <- c(terms_groups, list(
      "Transition" = seq_along(terms_transitions) + nterms
    ))
  }

  coefmap <- structure(
    vnames_new,
    names = vnames
  ) |> as.list()

  if (skip_intercept) {
    coefmap <- coefmap[!grepl("^Logit intercept nsi_[a-zA-Z_]+$", names(coefmap))]
  }

  list(
    custom.coef.map = coefmap,
    reorder.coef = terms_order,
    groups = terms_groups
  )

}

#' @param fits Either a single or a list of `defm` fit objects.
#' @param fun Function to be called from the `texreg` package, e.g., `texreg::screenreg`.
#' @param ... Further arguments passed to `summary_table` (with the exception of `as_texreg`, which is set to `TRUE`).
#' @param skip_intercept Whether or not to skip the intercept (logit) terms when printing the table
#' @rdname defm_mle
#' @returns
#' An object of class texreg with additional attributes: `custom.coef.map`,
#' `reorder.coef`, and `groups`.
#' @export
texreg_fancy <- function(
  fits,
  fun,
  skip_intercept = FALSE,
  ...
) {

  extra_info <- group_terms(fits, skip_intercept = skip_intercept)

  if (inherits(fits, "mle")) {

    fits <- list(fits)

  } else if (inherits(fits, "list")) {

    if (!all(sapply(fits, inherits, what = "mle"))) {

      stop("When `fits` is a list, all its elements should be of class 'mle'.")

    }

  }

  do.call(
    fun,
    c(
      list(lapply(fits, summary_table, as_texreg = TRUE, ...)),
      extra_info
      )
  )

}
#' Fitted DEFM
#'
#' Objects of class `defm_mle` are returned by [defm_mle()]. The class
#' extends [stats4::mle-class], so all its methods work. Printing the object
#' or its summary also suggests calling [summary_table()].
#'
#' @slot y_names,x_names Character vectors with the names of the outcomes
#' and covariates of the fitted model.
#' @param object An object of class `defm_mle` or `summary.defm_mle`.
#' @param ... Further arguments passed to the `summary` method of `mle`.
#' @importFrom methods setClass setMethod new callNextMethod show
#' @exportClass defm_mle summary.defm_mle
#' @aliases summary.defm_mle-class
#' @name defm_mle-class
setClass(
  "defm_mle",
  contains = "mle",
  slots    = c(y_names = "character", x_names = "character")
)

setClass("summary.defm_mle", contains = "summary.mle")

summary_table_hint <- function() {
  cat(
    "\nTip: call summary_table() on the fitted model for a compact table",
    "by outcome (console, HTML, or Markdown).\n"
  )
}

#' @rdname defm_mle-class
#' @exportMethod show
setMethod("show", "defm_mle", function(object) {
  methods::callNextMethod()
  summary_table_hint()
})

#' @rdname defm_mle-class
#' @exportMethod summary
setMethod("summary", "defm_mle", function(object, ...) {
  methods::new("summary.defm_mle", methods::callNextMethod())
})

#' @rdname defm_mle-class
setMethod("show", "summary.defm_mle", function(object) {
  methods::callNextMethod()
  summary_table_hint()
})
