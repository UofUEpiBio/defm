# Simulated data with three outcomes and two covariates
set.seed(1231)
n  <- 400
id <- rep(1:40, each = 10)
Y  <- matrix(
  rbinom(n * 3, 1, .4),
  ncol     = 3,
  dimnames = list(NULL, c("smoke", "drink", "vape"))
)
X <- cbind(female = rbinom(n, 1, .5), age = rnorm(n))

m <- new_defm(id, Y, X, order = 1)
td_logit_intercept(m)
td_logit_intercept(m, covar = "female")
td_logit_intercept(m, y_indices = c(0, 2), covar = "age")
td_formula(m, "{y0, 0y1} x age")
td_formula(m, "{y0_0, y1_0} > {0y0, 0y1}")
td_formula(m, "{y0_0} > {y0} x female")
init_defm(m)

fit <- defm_mle(m)

# The fit keeps working as an mle object and suggests summary_table()
expect_true(inherits(fit, "mle"))
expect_inherits(fit, "defm_mle")
expect_stdout(print(fit), "summary_table")
expect_stdout(show(summary(fit)), "summary_table")
expect_true(is.matrix(summary(fit)@coef))

tab <- summary_table(fit)
expect_inherits(tab, "defm_summary_table")

# Section 1: outcomes by covariates
logit <- tab$sections$logit
expect_equal(colnames(logit$est), c("smoke", "drink", "vape"))
expect_equal(rownames(logit$est), c("(Intercept)", "female", "age"))
expect_equal(
  logit$est["female", "smoke"],
  unname(coef(fit)["Logit intercept smoke x female"])
)
expect_true(is.na(logit$est["age", "drink"]))

# Section 2: motifs by interaction
markov <- tab$sections$markov
expect_equal(colnames(markov$est), c("Main", "x age", "x female"))
expect_equal(
  rownames(markov$est),
  c("{smoke, 0drink}", "{smoke, drink} > {0smoke, 0drink}", "{smoke} > {smoke}")
)
expect_equal(
  markov$est["{smoke, 0drink}", "x age"],
  unname(coef(fit)["Motif {smoke+, drink-} x age"])
)
expect_equal(
  markov$term["{smoke} > {smoke}", "x female"],
  "Motif {smoke+}>{smoke+} x female"
)

# Console output
out <- format(tab)
expect_true(any(grepl("Outcome-specific (logit) terms", out, fixed = TRUE)))
expect_true(any(grepl("Joint and transition (Markov) terms", out, fixed = TRUE)))
expect_true(any(grepl("^\\s+smoke\\s+drink\\s+vape$", out)))
expect_stdout(print(tab), "Log-likelihood")

# Standard errors below (default) or to the side
est_txt <- formatC(coef(fit)["Logit intercept smoke"], digits = 3, format = "f")
line    <- out[grepl("^\\(Intercept\\)", out)]
expect_false(grepl("\\(\\d", line))
line <- format(summary_table(fit, se = "side"))
line <- line[grepl("^\\(Intercept\\)", line)]
expect_true(grepl(est_txt, line, fixed = TRUE))
expect_true(grepl("\\(\\d", line))

# Digits
out1 <- format(summary_table(fit, digits = 1))
expect_true(any(grepl(
  formatC(coef(fit)["Logit intercept smoke"], digits = 1, format = "f"),
  out1[grepl("^\\(Intercept\\)", out1)],
  fixed = TRUE
)))

# Labels apply to outcomes, covariates, and motifs
tab_l <- summary_table(
  fit,
  labels = c(smoke = "Smoking", female = "Female", "(Intercept)" = "Baseline")
)
expect_equal(colnames(tab_l$sections$logit$est), c("Smoking", "drink", "vape"))
expect_equal(rownames(tab_l$sections$logit$est)[1:2], c("Baseline", "Female"))
expect_true("{Smoking} > {Smoking}" %in% rownames(tab_l$sections$markov$est))
expect_true(any(grepl("x Female", format(tab_l), fixed = TRUE)))

# Omitting terms
tab_o <- summary_table(fit, omit = "^Motif|vape")
expect_null(tab_o$sections$markov)
expect_equal(colnames(tab_o$sections$logit$est), c("smoke", "drink"))

# HTML and markdown
html <- format(tab, type = "html")
expect_true(grepl("^<table", html[1]))
expect_true(any(grepl("&gt;", html, fixed = TRUE)))
md <- format(tab, type = "markdown")
expect_true(any(grepl("|:---|", md, fixed = TRUE)))

# Old output and texreg
df <- as.data.frame(tab)
expect_equal(colnames(df), c("coef", "se", "pvalues"))
expect_equal(rownames(df), names(coef(fit)))
if (requireNamespace("texreg", quietly = TRUE)) {
  expect_inherits(summary_table(fit, as_texreg = TRUE), "texreg")
  m0 <- new_defm(id, Y, X, order = 0)
  td_logit_intercept(m0, covar = "female")
  init_defm(m0)
  fit0 <- defm_mle(m0)
  expect_stdout(
    print(texreg_fancy(list(fit0, fit0), texreg::screenreg)),
    "Model 2"
  )
}

# Same table without a valid model pointer, or without the stored names
fit2 <- unserialize(serialize(fit, NULL))
expect_equal(summary_table(fit2)$sections, tab$sections)
expect_equal(summary_table(as(fit, "mle"))$sections, tab$sections)
