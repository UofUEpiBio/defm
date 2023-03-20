## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib defm, .registration = TRUE
## usethis namespace: end
NULL

#' @rdname DEFM
#' @export
new_defm <- function(
    id, Y, X, order = 1
) {

  m <- new_defm_cpp(id, Y, X, order)

  cnames_y <- if (is.matrix(Y))
    colnames(Y)
  else
    NULL

  cnames_x <- if (is.matrix(X))
    colnames(X)
  else
    NULL

  if (is.null(cnames_y))
    cnames_y <- paste0("y", 0:(ncol(Y) - 1))

  if (is.null(cnames_x))
    cnames_x <- paste0("x", 0:(ncol(X) - 1))

  set_names(m, cnames_y, cnames_x)

  m

}

#' @export
#' @rdname defm_terms
#' @param e1,e2 e1 An object of class [DEFM] (e1) and a formula (e2).
#' @details The `+` method is a shortcut for term_formula
`+.DEFM` <- function(e1, e2) {
  if (!inherits(e2, "formula"))
    stop("The RHS should be a formula.")

  res <- strsplit(gsub("^\\s*~", "", deparse(e2)), split = "\\s*[+]\\s*")
  for (r in res)
    term_defm_transition_formula(e1, r)

  invisible(e1)

}
