# Exact gradient and Hessian of the DEFM log-likelihood ------------------------
# A small relative objective change must not hide a nonzero gradient. The
# large constant also makes objective rounding visible near the minimum.
plateau <- defm:::defm_newton(
  par = 1,
  fn = function(p) 1e8 + exp(p) - p,
  gr = function(p) exp(p) - 1,
  hess = function(p) matrix(exp(p)),
  control = list()
)
expect_equal(plateau$convergence, 0L)
expect_true(abs(exp(plateau$par) - 1) <= 1e-8)

data(valentesnsList)

mymodel <- new_defm(
  id    = valentesnsList$id,
  Y     = valentesnsList$Y,
  X     = valentesnsList$X,
  order = 1
)

td_logit_intercept(mymodel)
td_logit_intercept(mymodel, covar = "Hispanic")
td_logit_intercept(mymodel, covar = "exposure_smoke")
td_formula(mymodel, "{y1, 0y2} > {y1, y2}")
td_formula(mymodel, "{y0_0} > {y2} x Grades")
init_defm(mymodel)

K <- nterms_defm(mymodel)
set.seed(331)
par <- runif(K, -.5, .5)

# Central finite differences
h <- 1e-5
grad_fd <- sapply(seq_len(K), function(j) {
  e <- replace(double(K), j, h)
  (loglike_defm(mymodel, par + e) - loglike_defm(mymodel, par - e)) / (2 * h)
})

hess_fd <- sapply(seq_len(K), function(j) {
  e <- replace(double(K), j, h)
  (loglike_defm_grad(mymodel, par + e) -
    loglike_defm_grad(mymodel, par - e)) / (2 * h)
})

grad <- loglike_defm_grad(mymodel, par)
hess <- loglike_defm_hess(mymodel, par)

expect_equal(length(grad), K)
expect_equal(dim(hess), c(K, K))
expect_equal(grad, grad_fd, tolerance = 1e-5)
expect_equal(hess, hess_fd, tolerance = 1e-5)
expect_equal(hess, t(hess))
expect_true(all(eigen(hess, only.values = TRUE)$values < 0))
expect_equal(loglike_defm_grad(mymodel, par, ncores = 2), grad)

# defm_mle() matches the previous implementation (no gradient) ---------------
minuslog <- function(p) -loglike_defm(mymodel, p)
reference <- stats::optim(
  rep(0, K),
  minuslog,
  method  = "L-BFGS-B",
  lower   = -20,
  upper   = 20,
  hessian = TRUE
)

ans <- defm_mle(mymodel)

expect_inherits(ans, "defm_mle")
expect_equal(unname(coef(ans)), reference$par, tolerance = 1e-3)
expect_equal(
  unname(sqrt(diag(vcov(ans)))),
  sqrt(diag(solve(reference$hessian))),
  tolerance = 1e-3
)
expect_equal(names(coef(ans)), names(mymodel))
expect_equal(dimnames(vcov(ans)), list(names(mymodel), names(mymodel)))
expect_equal(
  as.numeric(logLik(ans)),
  loglike_defm(mymodel, unname(coef(ans)))
)
expect_true(max(abs(loglike_defm_grad(mymodel, unname(coef(ans))))) < 1e-2)

# Newton-Raphson and BFGS agree with L-BFGS-B
ans_newton <- defm_mle(mymodel, method = "newton")
ans_bfgs   <- defm_mle(mymodel, method = "BFGS")

expect_equal(ans_newton@details$convergence, 0L)
expect_equal(coef(ans_newton), coef(ans), tolerance = 1e-4)
expect_equal(coef(ans_bfgs), coef(ans), tolerance = 1e-4)
expect_true(
  max(abs(loglike_defm_grad(mymodel, unname(coef(ans_newton))))) < 1e-6
)

# Fixed parameters -------------------------------------------------------------
fixed_name <- names(mymodel)[K]
ans_fixed  <- defm_mle(
  mymodel,
  fixed  = structure(list(0.5), names = fixed_name),
  method = "newton"
)

# coef() returns the full vector, the @coef slot only the free parameters
expect_equal(length(ans_fixed@coef), K - 1L)
expect_equal(unname(coef(ans_fixed)[K]), 0.5)
expect_false(fixed_name %in% names(ans_fixed@coef))
expect_equal(dim(vcov(ans_fixed)), c(K - 1L, K - 1L))

reference_fixed <- stats::optim(
  rep(0, K - 1),
  function(p) -loglike_defm(mymodel, c(p, 0.5)),
  method = "BFGS"
)
expect_equal(
  unname(ans_fixed@coef),
  reference_fixed$par,
  tolerance = 1e-3
)

# Named and unnamed starting values
expect_equal(
  coef(defm_mle(mymodel, start = as.list(coef(ans)))),
  coef(ans),
  tolerance = 1e-5
)
expect_error(defm_mle(mymodel, start = list(notaterm = 1)), "not terms")

# Order-0 logit intercepts: same as glm() -----------------------------------
logit_0 <- new_defm(
  id    = valentesnsList$id,
  Y     = valentesnsList$Y[, 1, drop = FALSE],
  X     = valentesnsList$X,
  order = 0
)

td_logit_intercept(logit_0)
td_logit_intercept(logit_0, covar = "Hispanic")
td_logit_intercept(logit_0, covar = "Grades")
init_defm(logit_0)

ans_logit <- defm_mle(logit_0, method = "newton")
ans_glm   <- with(
  valentesnsList,
  glm(Y[, 1] ~ X[, "Hispanic"] + X[, "Grades"], family = binomial())
)

expect_equal(unname(coef(ans_logit)), unname(coef(ans_glm)), tolerance = 1e-6)
expect_equal(
  unname(sqrt(diag(vcov(ans_logit)))),
  unname(sqrt(diag(vcov(ans_glm)))),
  tolerance = 1e-5
)

# Refitting through the stored call, as stats4::profile() does
refit_call <- ans_logit@call
refit_call$minuslogl <- ans_logit@minuslogl
refit_call$fixed     <- list(`Logit intercept alcohol` = -1)
refit_call$start     <- ans_logit@fullcoef[-1]
refit <- eval(refit_call)

expect_inherits(refit, "defm_mle")
expect_equal(unname(coef(refit)[1]), -1)
expect_true(as.numeric(logLik(refit)) < as.numeric(logLik(ans_logit)))
