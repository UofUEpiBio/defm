#' Motif census
#' 
#' Calculates the total motif counts for a given model, in terms
#' of the number of times each motif appears in the data.
#' 
#' @param m An object of class [DEFM].
#' @param locs Idx (starting from zero) with the variables that will be
#' included in the census.
#' @export
#' @returns A matrix of class [defm_motif_census] with the motif counts.
#' @examples
#' # Loading Valente's SNS data
#' data(valentesnsList)
#' 
#' mymodel <- new_defm(
#'   id = valentesnsList$id,
#'   Y = valentesnsList$Y,
#'   X = valentesnsList$X,
#'   order = 1
#' )
#' 
#' # Adding the intercept terms and a motif from tobacco to mj
#' term_defm_logit_intercept(mymodel)
#' term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")
#' 
#' # Initialize the model
#' init_defm(mymodel)
#' 
#' # Motif counts featuring only the first two variables
#' motif_census(mymodel, locs = 0:1)
#' @name motif_census
#' @aliases defm_motif_census
#' @references
#' Vega Yon, G. G., Pugh, M. J., & Valente, T. W. (2022). Discrete Exponential-Family Models for Multivariate Binary Outcomes (arXiv:2211.00627). arXiv. \url{https://arxiv.org/abs/2211.00627}
motif_census <- function(m, locs) {

  # No repeated values
  locs <- sort(unique(locs))

  structure(
    motif_census_cpp(m, locs),
    class  = "defm_motif_census",
    locs   = locs,
    labels = get_Y_names(m)[locs + 1],
    order  = morder_defm(m)
  )

}

#' @export 
as.data.frame.defm_motif_census <- function(
  x, row.names = NULL, optional = FALSE, ...
  ) {

  # Getting parameters
  m      <- attr(x, "order")
  locs   <- attr(x, "locs")
  labels <- attr(x, "labels")
  k      <- length(labels)
  ord    <- order(x[,1], decreasing = TRUE)

  colidx <- list((1:k) + 1, (k + 2):ncol(x))

  tmp <- NULL
  for (i in ord) {

    from <- paste(x[i, colidx[[1]]], collapse = ", ")
    to   <- paste(x[i, colidx[[2]]], collapse = ", ")

    tmp <- rbind(tmp, data.frame(
      Motif = sprintf("{%s} > {%s}%s", from, to, ifelse(from == to, " *", "")),
      Total = x[i, 1L]
    ))

  }

  rownames(tmp) <- NULL

  tmp

}

#' @export
print.defm_motif_census <- function(x, ...) {

  # Building label
  labels <- attr(x, "labels")
  if (any(grepl("\\s+", labels))) {
    lab <- paste0("{`", paste(labels, collapse = "`, `"), "`}")
  } else {
    lab <- paste0("{", paste(labels, collapse = ", "), "}")
  }

  cat(sprintf("Motif census for variable set: %s\n", lab))
  print(as.data.frame(x))
  cat("(*): No change\n")

  invisible(x)

}

#' @export
names.defm_motif_census <- function(x) {
  attr(x, "labels")
}
