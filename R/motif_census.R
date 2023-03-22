#' Motif census
#' @param m An object of class [DEFM].
#' @param locs Idx (starting from zero) with the variables that will be
#' included in the census.
#' @export
#' @name motif_census
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
as.data.frame.defm_motif_census <- function(x, row.names = NULL, optional = FALSE, ...) {

  # Getting parameters
  m      <- attr(x, "order")
  locs   <- attr(x, "locs")
  labels <- attr(x, "labels")
  k      <- length(labels)
  ord    <- order(x[,1], decreasing = TRUE)

  colidx <- list((1:k) + 1, (k + 2):ncol(x))

  tmp <- NULL
  for (i in 1L:nrow(x)) {

    tmp <- rbind(tmp, data.frame(
      Motif = sprintf(
        "{%s} > {%s}",
        paste(x[i, colidx[[1]]], collapse = ", "),
        paste(x[i, colidx[[2]]], collapse = ", ")
      ),
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

  return(x)

}

#' @export
names.defm_motif_census <- function(x) {
  attr(x, "labels")
}
