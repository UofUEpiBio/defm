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

# Printing the fit (and its summary) suggests summary_table()
expect_stdout(print(fit), "summary_table")
expect_stdout(show(summary(fit)), "summary_table")

tab <- summary_table(fit)

# Section 1: outcomes by covariates, empty when there is no term
logit <- tab$sections$logit
expect_equal(
  dimnames(logit$est),
  list(c("(Intercept)", "female", "age"), c("smoke", "drink", "vape"))
)
expect_equal(
  logit$est["female", "smoke"],
  unname(coef(fit)["Logit intercept smoke x female"])
)
expect_true(is.na(logit$est["age", "drink"]))

# Section 2: motifs (td_formula notation) by interaction
markov <- tab$sections$markov
expect_equal(
  dimnames(markov$est),
  list(
    c("{smoke, 0drink}", "{smoke, drink} > {0smoke, 0drink}", "{smoke} > {smoke}"),
    c("Main", "x age", "x female")
  )
)
expect_equal(
  markov$est["{smoke, 0drink}", "x age"],
  unname(coef(fit)["Motif {smoke+, drink-} x age"])
)

# Console output, standard errors below (default) or to the side, digits
out <- format(tab)
expect_true(all(c(
  "Outcome-specific (logit) terms",
  "Joint and transition (Markov) terms"
) %in% out))
expect_false(grepl("\\(\\d", out[grepl("^\\(Intercept\\)", out)]))

out <- format(summary_table(fit, se = "side", digits = 1))
expect_true(grepl(
  paste0(
    formatC(coef(fit)["Logit intercept smoke"], digits = 1, format = "f"),
    "\\S*\\s+\\(\\d\\.\\d\\)"
  ),
  out[grepl("^\\(Intercept\\)", out)]
))

# Labels apply to outcomes, covariates, and motifs; omit drops terms
tab_l <- summary_table(
  fit,
  labels = c(smoke = "Smoking", female = "Female"),
  omit   = "vape"
)
expect_equal(
  list(colnames(tab_l$sections$logit$est), colnames(tab_l$sections$markov$est)),
  list(c("Smoking", "drink"), c("Main", "x age", "x Female"))
)
expect_true("{Smoking} > {Smoking}" %in% rownames(tab_l$sections$markov$est))

# HTML and markdown
expect_true(
  grepl("^<table", format(tab, type = "html")[1]) &&
    any(grepl("|:---|", format(tab, type = "markdown"), fixed = TRUE))
)

# The previous output is still available
expect_equal(colnames(as.data.frame(tab)), c("coef", "se", "pvalues"))

# Same table without a valid model pointer
fit2 <- unserialize(serialize(fit, NULL))
expect_equal(summary_table(fit2)$sections, tab$sections)
