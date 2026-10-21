#' Summary table of a fitted DEFM
#'
#' Builds a compact table of the estimates of a model fitted with
#' [defm_mle()]. Instead of listing the terms one after the other, the
#' table has two sections:
#'
#' 1. **Outcome-specific (logit) terms**: a matrix with one column per
#'    outcome and one row per covariate interaction (`(Intercept)` for the
#'    main logit term). Cells are empty when the model does not include
#'    that combination.
#' 2. **Joint and transition (Markov) terms**: one row per motif
#'    (co-occurrence, e.g., `{y0, y1}`, and transitions, e.g.,
#'    `{y0} > {y0, y1}`) and one column per interaction (`Main` for the
#'    term without interaction). Other terms, e.g., [td_ones()], are also
#'    reported here.
#'
#' Motifs are written using the [td_formula()] notation: a leading `0`
#' marks an outcome equal to zero.
#'
#' @param object An object returned by [defm_mle()].
#' @param as_texreg When `TRUE`, returns a texreg object with all the terms
#' listed one by one (see [texreg_fancy()]). All other formatting arguments
#' are ignored.
#' @param labels Named character vector (or list) to rename the elements of
#' the table, e.g., `c(y0 = "Smoking", x1 = "Female")`. Names can be
#' outcome names, covariate names, `"(Intercept)"`, `"Main"`, or the name of
#' a term without the covariate interaction (e.g., `"Num. of ones"`).
#' Outcome names are also replaced within motifs.
#' @param omit Regular expression. Terms whose (original) name matches it
#' are excluded from the table.
#' @param digits Integer. Number of decimals for the estimates and standard
#' errors.
#' @param se Either `"below"` (standard errors in a separate row under the
#' estimate) or `"side"` (standard errors next to the estimate).
#' @param stars Numeric vector of p-value thresholds for the significance
#' stars. The smallest value gets the most stars. `NULL` omits the stars.
#' @param model Optional [DEFM] object used to fit the model. Used to
#' retrieve the names of the outcomes and covariates. Only needed for fits
#' that do not store them (see details).
#' @param ... Ignored.
#'
#' @details
#' The table is printed to the console as plain text. Use [format()] with
#' `type = "html"` or `type = "markdown"` to get the table as HTML or as a
#' Markdown pipe table. Within Quarto and R Markdown documents, the table
#' is rendered automatically (HTML for HTML output and Markdown otherwise).
#'
#' The terms are classified by their names. Fits returned by [defm_mle()]
#' store the outcome and covariate names, which makes the classification
#' exact. For other `mle` objects, the names are taken from `model` or, if
#' possible, from the model object used in the fit.
#'
#' @returns
#' - `summary_table()` returns an object of class `defm_summary_table`, a
#'   list with the elements `sections` (for each section, matrices with the
#'   estimates `est`, standard errors `se`, p-values `p`, and the original
#'   term names `term`), `coefs` (the full table of coefficients), `gof`
#'   (goodness-of-fit statistics), and `settings`. If `as_texreg = TRUE`,
#'   a texreg object.
#' - `format()` returns a character vector with the lines of the table.
#' - `as.data.frame()` returns a data frame with the columns `coef`, `se`,
#'   and `pvalues`, one row per term (the format returned by
#'   `summary_table()` in earlier versions of the package).
#'
#' @examples
#' data(valentesnsList)
#'
#' mymodel <- new_defm(
#'   id    = valentesnsList$id,
#'   Y     = valentesnsList$Y,
#'   X     = valentesnsList$X,
#'   order = 0
#' )
#'
#' td_logit_intercept(mymodel)
#' td_logit_intercept(mymodel, covar = "Hispanic")
#' td_formula(mymodel, "{y0, y1}")
#' init_defm(mymodel)
#'
#' ans <- defm_mle(mymodel)
#'
#' summary_table(ans)
#'
#' # Renaming, standard errors to the side, and two decimals
#' summary_table(
#'   ans,
#'   labels = c(mj = "Marijuana", Hispanic = "Hispanic (yes)"),
#'   se     = "side",
#'   digits = 2
#' )
#'
#' # HTML and Markdown versions
#' format(summary_table(ans), type = "html")
#' format(summary_table(ans), type = "markdown")
#'
#' # The terms one by one
#' as.data.frame(summary_table(ans))
#' @export
summary_table <- function(
    object,
    as_texreg = FALSE,
    labels    = NULL,
    omit      = NULL,
    digits    = 3L,
    se        = c("below", "side"),
    stars     = c(0.001, 0.01, 0.05),
    model     = NULL,
    ...
) {

  coefs <- stats4::coef(object)
  ses   <- sqrt(diag(stats4::vcov(object)))
  pvals <- pval_calc(object)

  gof <- c(
    N      = stats4::nobs(object),
    AIC    = stats4::AIC(object),
    BIC    = stats4::BIC(object),
    logLik = as.numeric(stats4::logLik(object))
  )

  if (as_texreg) {

    if (!length(find.package("texreg", quiet = TRUE)))
      stop("To use texreg you need to have the package installed in your system. Currently it seems to be not available.")

    return(texreg::createTexreg(
      coef.names  = names(coefs),
      coef        = coefs,
      se          = ses,
      gof.names   = c("AIC", "BIC", "N"),
      gof         = unname(gof[c("AIC", "BIC", "N")]),
      gof.decimal = c(TRUE, TRUE, FALSE),
      pvalues     = pvals
    ))

  }

  se <- match.arg(se)

  if (!is.null(stars))
    stars <- sort(stars)

  if (!is.null(labels))
    labels <- unlist(labels)

  # Selecting the terms
  keep <- rep(TRUE, length(coefs))
  if (!is.null(omit))
    keep <- !grepl(omit, names(coefs))

  vnames <- defm_fit_names(object, model)
  terms  <- parse_defm_terms(names(coefs)[keep], vnames$y, vnames$x)

  sections <- build_sections(
    terms,
    est    = coefs[keep],
    se     = ses[keep],
    p      = pvals[keep],
    y      = vnames$y,
    labels = labels
  )

  structure(
    list(
      sections = sections,
      coefs    = data.frame(coef = coefs, se = ses, pvalues = pvals),
      gof      = gof,
      settings = list(digits = digits, se = se, stars = stars)
    ),
    class = "defm_summary_table"
  )

}

# Retrieves the names of the outcomes and covariates of a fit
defm_fit_names <- function(object, model = NULL) {

  get_names <- function(m) {
    tryCatch(
      list(
        y = as.character(get_Y_names(m)),
        x = as.character(get_X_names(m))
      ),
      error = function(e) NULL
    )
  }

  ans <- NULL
  if (!is.null(model)) {
    ans <- get_names(model)
  } else if (methods::is(object, "defm_mle")) {
    ans <- list(y = object@y_names, x = object@x_names)
  } else if (methods::is(object, "mle")) {
    # defm_mle() creates the objective function in its own frame
    m <- tryCatch(
      get("object", envir = environment(object@minuslogl), inherits = FALSE),
      error = function(e) NULL
    )
    if (inherits(m, "DEFM"))
      ans <- get_names(m)
  }

  if (is.null(ans))
    ans <- list(y = character(0), x = character(0))

  ans

}

# Classifies the terms into the logit and markov sections
parse_defm_terms <- function(tnames, y_names, x_names) {

  n     <- length(tnames)
  base  <- tnames
  covar <- rep(NA_character_, n)

  # Covariate interactions are added as " x [covariate name]"
  if (length(x_names)) {

    for (x in x_names[order(-nchar(x_names))]) {
      sfx <- paste0(" x ", x)
      hit <- is.na(covar) & endsWith(tnames, sfx) &
        (nchar(tnames) > nchar(sfx))
      covar[hit] <- x
      base[hit]  <- substr(tnames[hit], 1L, nchar(tnames[hit]) - nchar(sfx))
    }

  } else {

    pattern <- "^(.+) x ([^{}>]+)$"
    hit <- grepl(pattern, tnames)
    covar[hit] <- sub(pattern, "\\2", tnames[hit])
    base[hit]  <- sub(pattern, "\\1", tnames[hit])

  }

  # Logit intercepts and single positive motifs are outcome specific
  outcome  <- rep(NA_character_, n)
  is_logit <- startsWith(base, "Logit intercept ")
  outcome[is_logit] <- substring(base[is_logit], nchar("Logit intercept ") + 1L)

  pattern <- "^Motif \\{([^{},>]+)\\+\\}$"
  is_single <- grepl(pattern, base)
  outcome[is_single] <- sub(pattern, "\\1", base[is_single])

  if (length(y_names))
    outcome[!(outcome %in% y_names)] <- NA_character_

  data.frame(
    term             = tnames,
    base             = base,
    covar            = covar,
    outcome          = outcome,
    section          = ifelse(is.na(outcome), "markov", "logit"),
    stringsAsFactors = FALSE
  )

}

# Applies the labels to a character vector
apply_labels <- function(x, labels) {

  if (is.null(labels) || !length(x))
    return(x)

  hit <- x %in% names(labels)
  x[hit] <- labels[x[hit]]
  x

}

# Rewrites a motif name, e.g., "Motif {y0+, y1-}>{y0+}", as
# "{y0, 0y1} > {y0}"
pretty_motif <- function(x, labels) {

  if (!is.null(labels) && (x %in% names(labels)))
    return(unname(labels[x]))

  if (!grepl("^Motif \\{.*\\}$", x))
    return(x)

  parts <- strsplit(sub("^Motif\\s+", "", x), "\\}\\s*>\\s*\\{")[[1]]
  parts <- gsub("[{}]", "", parts)

  parts <- vapply(parts, function(p) {
    tok <- trimws(strsplit(p, ",")[[1]])
    neg <- endsWith(tok, "-")
    nms <- apply_labels(sub("[+-]$", "", tok), labels)
    paste0("{", paste0(ifelse(neg, "0", ""), nms, collapse = ", "), "}")
  }, character(1))

  paste(parts, collapse = " > ")

}

# Creates the matrices of estimates for each section
build_sections <- function(terms, est, se, p, y, labels) {

  terms$row <- ifelse(
    terms$section == "logit",
    ifelse(is.na(terms$covar), "(Intercept)", terms$covar),
    terms$base
  )

  terms$col <- ifelse(
    terms$section == "logit",
    terms$outcome,
    ifelse(is.na(terms$covar), "Main", terms$covar)
  )

  # A logit cell can only hold one term (e.g., both `Logit intercept y0`
  # and `Motif {y0+}`); the rest are reported as markov terms.
  key <- paste(terms$section, terms$row, terms$col, sep = "\r")
  dup <- duplicated(key) & (terms$section == "logit")
  if (any(dup)) {
    terms$section[dup] <- "markov"
    terms$row[dup]     <- terms$base[dup]
    terms$col[dup]     <- ifelse(is.na(terms$covar[dup]), "Main", terms$covar[dup])
  }

  sections <- list()
  for (s in c("logit", "markov")) {

    idx <- which(terms$section == s)
    if (!length(idx))
      next

    rows <- unique(terms$row[idx])
    cols <- unique(terms$col[idx])

    if (s == "logit") {
      if ("(Intercept)" %in% rows)
        rows <- c("(Intercept)", setdiff(rows, "(Intercept)"))
      if (length(y))
        cols <- intersect(y, cols)
      row_labels <- apply_labels(rows, labels)
      col_labels <- apply_labels(cols, labels)
    } else {
      if ("Main" %in% cols)
        cols <- c("Main", setdiff(cols, "Main"))
      row_labels <- vapply(rows, pretty_motif, character(1), labels = labels)
      col_labels <- ifelse(
        cols == "Main",
        apply_labels(cols, labels),
        paste("x", apply_labels(cols, labels))
      )
    }

    empty <- matrix(
      NA_real_,
      nrow     = length(rows),
      ncol     = length(cols),
      dimnames = list(unname(row_labels), unname(col_labels))
    )

    sec <- list(est = empty, se = empty, p = empty, term = empty)
    sec$term[] <- NA_character_

    ij <- cbind(
      match(terms$row[idx], rows),
      match(terms$col[idx], cols)
    )

    sec$est[ij]  <- est[idx]
    sec$se[ij]   <- se[idx]
    sec$p[ij]    <- p[idx]
    sec$term[ij] <- terms$term[idx]

    sections[[s]] <- sec

  }

  sections

}

summary_table_titles <- c(
  logit  = "Outcome-specific (logit) terms",
  markov = "Joint and transition (Markov) terms"
)

# Formats a section as a character matrix (row names are the labels)
format_section <- function(sec, settings, pad = FALSE) {

  fmt <- function(v) formatC(v, digits = settings$digits, format = "f")

  nstars <- length(settings$stars)
  stars  <- if (nstars) {
    nst <- vapply(
      sec$p,
      function(pv) if (is.na(pv)) 0L else sum(pv < settings$stars),
      integer(1)
    )
    strrep("*", nst)
  } else
    rep("", length(sec$p))

  if (pad && nstars)
    stars <- paste0(stars, strrep(" ", nstars - nchar(stars)))

  est_cells <- paste0(fmt(sec$est), stars)
  se_cells  <- paste0(
    "(", fmt(sec$se), ")",
    if (pad && nstars > 1L) strrep(" ", nstars - 1L) else ""
  )

  missing <- is.na(sec$est)

  if (settings$se == "side") {

    cells <- paste(paste0(fmt(sec$est), stars), paste0("(", fmt(sec$se), ")"))
    cells[missing] <- ""
    cells <- matrix(cells, nrow = nrow(sec$est), dimnames = dimnames(sec$est))

  } else {

    est_cells[missing] <- ""
    se_cells[missing]  <- ""

    nr <- nrow(sec$est)
    cells <- matrix("", nrow = 2L * nr, ncol = ncol(sec$est))
    cells[seq(1L, by = 2L, length.out = nr), ] <- est_cells
    cells[seq(2L, by = 2L, length.out = nr), ] <- se_cells

    rnames <- rep("", 2L * nr)
    rnames[seq(1L, by = 2L, length.out = nr)] <- rownames(sec$est)
    dimnames(cells) <- list(rnames, colnames(sec$est))

  }

  cells

}

# Footer lines: goodness of fit and the stars legend
summary_table_footer <- function(x) {

  d <- x$settings$digits
  g <- x$gof

  ans <- sprintf(
    "N = %d; AIC = %s; BIC = %s; Log-likelihood = %s",
    as.integer(g["N"]),
    formatC(g["AIC"], digits = d, format = "f"),
    formatC(g["BIC"], digits = d, format = "f"),
    formatC(g["logLik"], digits = d, format = "f")
  )

  stars <- x$settings$stars
  if (length(stars)) {
    ns <- length(stars)
    ans <- c(ans, paste(
      sprintf("%s p < %s", strrep("*", ns - seq_len(ns) + 1L), stars),
      collapse = "; "
    ))
  }

  ans

}

# Pads strings to a given display width
pad_str <- function(x, width, left = TRUE) {

  spaces <- strrep(" ", pmax(width - nchar(x, type = "width"), 0L))
  if (left) paste0(x, spaces) else paste0(spaces, x)

}

format_text <- function(x) {

  cells <- lapply(x$sections, format_section, settings = x$settings, pad = TRUE)

  w1 <- max(unlist(lapply(cells, \(m) nchar(rownames(m), type = "width"))), 0L)

  body <- lapply(cells, \(m) {
    widths <- vapply(seq_len(ncol(m)), \(j) {
      max(nchar(c(colnames(m)[j], m[, j]), type = "width"))
    }, integer(1))

    make_line <- \(lab, vals) {
      paste(
        c(pad_str(lab, w1), pad_str(vals, widths, left = FALSE)),
        collapse = "  "
      )
    }

    list(
      header = make_line("", colnames(m)),
      rows   = vapply(seq_len(nrow(m)), \(i) {
        make_line(rownames(m)[i], m[i, ])
      }, character(1))
    )
  })

  footer <- summary_table_footer(x)

  width <- max(
    nchar(unlist(body), type = "width"),
    nchar(summary_table_titles[names(body)], type = "width"),
    nchar(footer, type = "width")
  )

  ans <- character(0)
  for (s in names(body)) {
    ans <- c(
      ans,
      strrep("=", width),
      summary_table_titles[[s]],
      strrep("-", width),
      body[[s]]$header,
      strrep("-", width),
      body[[s]]$rows
    )
  }

  sub("\\s+$", "", c(ans, strrep("=", width), footer))

}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

format_html <- function(x) {

  cells <- lapply(x$sections, format_section, settings = x$settings)
  ncols <- 1L + max(vapply(cells, ncol, integer(1)), 0L)

  td <- function(v, style = "", tag = "td") {
    sprintf('<%s style="padding: 2px 8px;%s">%s</%1$s>', tag, style, v)
  }

  # Sections can have fewer columns than the widest one
  fill <- function(n) {
    if (n > 0L) paste(rep("<td></td>", n), collapse = "") else ""
  }

  ans <- '<table class="defm-summary-table" style="margin: 10px auto; border-collapse: collapse; border-spacing: 0px; border-top: 2px solid #000000; border-bottom: 2px solid #000000;">'

  for (s in names(cells)) {

    m <- cells[[s]]
    ans <- c(
      ans,
      sprintf(
        '<tr><th colspan="%d" style="padding: 6px 8px 2px 8px; text-align: left; font-style: italic;">%s</th></tr>',
        ncols,
        html_escape(summary_table_titles[[s]])
      ),
      paste0(
        '<tr style="border-bottom: 1px solid #000000;">',
        td("", tag = "th"),
        paste(
          td(html_escape(colnames(m)), " text-align: center;", tag = "th"),
          collapse = ""
        ),
        fill(ncols - 1L - ncol(m)),
        "</tr>"
      )
    )

    for (i in seq_len(nrow(m))) {
      ans <- c(ans, paste0(
        "<tr>",
        td(html_escape(rownames(m)[i]), " text-align: left;"),
        paste(td(html_escape(m[i, ]), " text-align: center;"), collapse = ""),
        fill(ncols - 1L - ncol(m)),
        "</tr>"
      ))
    }

  }

  c(
    ans,
    sprintf(
      '<tr><td colspan="%d" style="padding: 4px 8px; border-top: 1px solid #000000; font-size: 0.8em;">%s</td></tr>',
      ncols,
      paste(html_escape(summary_table_footer(x)), collapse = "<br>")
    ),
    "</table>"
  )

}

format_markdown <- function(x) {

  esc <- function(v) {
    v <- gsub("|", "\\|", v, fixed = TRUE)
    gsub("*", "\\*", v, fixed = TRUE)
  }

  cells <- lapply(x$sections, format_section, settings = x$settings)

  ans <- character(0)
  for (s in names(cells)) {

    m <- cells[[s]]
    row <- \(lab, vals) paste0("| ", paste(c(lab, vals), collapse = " | "), " |")

    ans <- c(
      ans,
      sprintf("**%s**", summary_table_titles[[s]]),
      "",
      row("", esc(colnames(m))),
      paste0("|:---|", paste(rep("---:", ncol(m)), collapse = "|"), "|"),
      vapply(seq_len(nrow(m)), \(i) {
        row(esc(rownames(m)[i]), esc(m[i, ]))
      }, character(1)),
      ""
    )

  }

  c(ans, paste0(esc(summary_table_footer(x)), "  "))

}

#' @export
#' @rdname summary_table
#' @param x An object of class `defm_summary_table`.
#' @param type Character. Either `"text"`, `"html"`, or `"markdown"`.
format.defm_summary_table <- function(
    x,
    type = c("text", "html", "markdown"),
    ...
) {

  type <- match.arg(type)

  switch(
    type,
    text     = format_text(x),
    html     = format_html(x),
    markdown = format_markdown(x)
  )

}

#' @export
#' @rdname summary_table
print.defm_summary_table <- function(x, ...) {

  cat(format(x, type = "text"), sep = "\n")
  invisible(x)

}

#' @export
#' @rdname summary_table
#' @param row.names,optional Ignored.
as.data.frame.defm_summary_table <- function(
    x,
    row.names = NULL,
    optional  = FALSE,
    ...
) {

  x$coefs

}

#' @exportS3Method knitr::knit_print
knit_print.defm_summary_table <- function(x, ...) {

  type <- if (knitr::is_html_output()) "html" else "markdown"

  knitr::asis_output(
    paste(c("", format(x, type = type), ""), collapse = "\n")
  )

}
