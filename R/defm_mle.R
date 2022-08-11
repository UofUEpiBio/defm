#' Maximum Likelihood Estimation of DEFM
#' @param object An object of class [DEFM].
#' @param start Double vector. Starting point for the MLE.
#' @param lower,upper Lower and upper limits for the optimization (passed to
#' [stats4::mle].)
#' @param ... Further arguments passed to [stats4::mle].
#' @export
#' @import stats4
defm_mle <- function(
  object,
  start,
  lower,
  upper,
  ...
  ) {

  if (missing(start))
    start <- as.list(structure(rep(0, nterms_defm(object)), names = names(object) ))

  if (missing(lower))
    lower <- rep(-20, length(nterms_defm(object)))

  if (missing(upper))
    upper <- rep(20, length(nterms_defm(object)))

  if (!inherits(object, "DEFM"))
    stop("-object- must be an object of class \"DEFM\"")

  minuslog <- sprintf(
    "function(%s) {
      par <- c(%1$s)
      -loglike_defm(object, par, as_log = TRUE)
    }", paste0("`", names(start), "`", collapse = ", ")
  )

  minuslog <- gsub("\\\\", "\\\\\\", minuslog)

  minuslog <- eval(parse(text = minuslog))

  stats4::mle(
    minuslogl = minuslog,
    start     = start,
    lower     = lower,
    upper     = upper,
    method    = "L-BFGS-B",
    nobs      = nrow_defm(object) + ifelse(
      morder_defm(object) > 0, -nobs_defm(object), 0L
      ),
    ...
  )

}

pval_calc <- function(obj) {
  pvals <- -abs(coef(obj)/sqrt(diag(vcov(obj)))) |> pnorm()
  pvals * 2
}

#' @export
#' @param as_texreg When `TRUE`, wraps the result in a texreg object
#' @details The function `summary_table` computes pvalues and returns a table
#' with the estimates, se, and pvalues. If `as_texreg = TRUE`, then it will
#' return a texreg object.
#' @rdname defm_mle
summary_table <- function(object, as_texreg = FALSE, ...) {

  # Generating the output table
  tab <- list(
    coef.names  = names(coef(object)),
    coef        = coef(object),
    se          = sqrt(diag(vcov(object))),
    gof.names   = c("AIC", "BIC", "N"),
    gof         = c(AIC(object), BIC(object), nobs(object)),
    gof.decimal = c(T,T,F),
    pvalues     = pval_calc(object)
  )

  if (as_texreg) {

    if (!length(find.package("texreg", quiet = TRUE)))
      stop("To use texreg you need to have the package installed in your system. Currently it seems to be not available.")

    do.call(texreg::createTexreg, tab)

  } else {

    do.call(
      data.frame,
      tab[c("coef", "se", "pvalues")]
      )

  }



}
