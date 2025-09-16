#' @export
print.DEFM_counters <- function(x, ...) {
  print_defm_counters(x)
}

#' @export
print.DEFM_counter <- function(x, ...) {
  print_defm_counter(x)
}

#' @rdname get_counters
#' @export
as.list.DEFM_counters <- function(x, ...) {

  n_counters <- length(x)
  lapply(seq_len(n_counters) - 1, \(i) {
    as.list(x[i])
  })

}

#' @export
#' @rdname get_counters
#' @returns
#' - The `as.list` methods return a list with the name and description
#' of the counters.
as.list.DEFM_counter <- function(x, ...) {

  as_list_defm_counter(x)

}