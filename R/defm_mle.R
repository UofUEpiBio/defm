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
    nobs      = nrow_defm(object),
    ...
  )
}
