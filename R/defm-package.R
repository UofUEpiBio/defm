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
