# Regression tests for the formula parser (USCbiostats/barry#26): covariate
# names are not motif terms, `}x Covar` keeps the covariate, and the LHS of a
# transition cannot include the current time.
#
# The fix lives in barry's headers (LinkingTo), so these tests only apply when
# defm was built against barry >= 0.2.2.9000.
if (utils::packageVersion("barry") < "0.2.2.9000")
  exit_file("barry < 0.2.2.9000 does not include the formula parser fix.")

set.seed(1)
n  <- 40
id <- rep(1:10, each = 4)
Y  <- matrix(
  rbinom(n * 2, 1, .5), ncol = 2,
  dimnames = list(NULL, c("a", "b"))
  )
X  <- cbind(Day1 = runif(n), Female = rbinom(n, 1, .5))

new_model <- function() new_defm(id, Y, X, order = 1)

# Covariate names containing y<digit>
m <- new_model()
td_formula(m, "{y0} > {y0} x Day1")
td_formula(m, "{y0} > {y1} x Day1")
td_formula(m, "{y0, 0y1} x Day1")
expect_equal(
  names(m),
  c(
    "Motif {a+}>{a+} x Day1",
    "Motif {a+}>{b+} x Day1",
    "Motif {a+, b-} x Day1"
  )
)

# The statistic is weighted by the covariate
init_defm(m)
S    <- get_stats(m)
prev <- rbind(NA, Y[-n, ])
prev[c(TRUE, id[-1] != id[-n]), ] <- NA
expect_equal(
  unname(S[, 1]),
  ifelse(is.na(prev[, 1]), NA, (prev[, 1] == 1 & Y[, 1] == 1) * X[, "Day1"])
)

# No space before `x` keeps the covariate
m <- new_model()
td_formula(m, "{y0} > {y1}x Female")
expect_equal(names(m), "Motif {a+}>{b+} x Female")

# The LHS cannot include the current time
m <- new_model()
expect_error(td_formula(m, "{y0_1} > {y1}"), "LHS")
expect_error(td_formula(m, "{y0_5} > {y1}"), "out of range")
