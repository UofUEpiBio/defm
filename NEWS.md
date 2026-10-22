# defm 0.3.0.0 (development version)

* `defm_mle()` is much faster for models with many terms: it now passes the
  exact gradient to `optim()` and computes the variance-covariance matrix from
  the exact Hessian, instead of using finite differences (through
  `stats4::mle()`). New arguments: `method` (`"L-BFGS-B"`, `"BFGS"`, or
  `"newton"`), `fixed`, `ncores`, and `control`. Refits through the stored
  call work, so `profile()` and `confint()` can be used.

* New functions `loglike_defm_grad()` and `loglike_defm_hess()` return the
  exact gradient and Hessian of the log-likelihood. `loglike_defm()` gains an
  `ncores` argument. These require a version of the barry headers with the
  new derivative methods (USCbiostats/barry#27).

* `summary_table()` now returns a compact two-section table: outcome-specific
  (logit) terms as an outcome-by-covariate matrix, and joint/transition
  (Markov) terms as a motif-by-interaction matrix. It prints to the console
  and can be formatted as HTML or Markdown (`format(x, type = ...)`); within
  Quarto and R Markdown it renders automatically. Options include `labels`
  (rename outcomes, covariates, and terms), `omit`, `digits`, `se`
  (`"below"` or `"side"`), and `stars`.

* **Breaking**: `summary_table()` no longer returns a `data.frame`. Use
  `as.data.frame(summary_table(fit))` to get the previous output (columns
  `coef`, `se`, and `pvalues`). `as_texreg = TRUE` is unchanged.

* `defm_mle()` now returns an object of class `defm_mle`, which extends
  `stats4::mle` and stores the outcome and covariate names. Printing a fit or
  its `summary()` now suggests calling `summary_table()`.

* `texreg_fancy()` failed when passed a list of fits; fixed.

* Building against `barry` (>= 0.2.2.9000), currently available from GitHub
  (USCbiostats/barryr), fixes the `td_formula()` parser (USCbiostats/barry#26).
  The CRAN version of `barry` still has these bugs:
  - Covariate names containing `y<digit>` (e.g., `Day1`) were read as motif
    terms, silently changing the model: `"{y0} > {y0} x Day1"` became
    `Motif {a+}>{a+, b+} x Day1`.
  - `"{...}x Covar"` (no space before `x`) silently dropped the covariate.
  - Transitions such as `"{y0_1} > {y1}"`, with a current-time term on the
    left-hand side, were silently turned into intercept motifs; they now
    throw an error.

* The same `barry` version also fixes wrong draws in `sim_defm()` caused by a
  support-vs-array index confusion (USCbiostats/barry#25).


# defm 0.2.2.0

* Requires `barry` (>= 0.2.2), which fixes a hash-collision bug that made
  `init_defm()` appear to hang for models with many outcome columns
  (USCbiostats/barry#24). A 16-outcome model that previously never finished
  initializing now takes seconds.

* Long-running computations (e.g., the support enumeration in `init_defm()`)
  can now be interrupted from R with Ctrl-C. Note that a model interrupted
  mid-initialization is left partially initialized and should be rebuilt
  with `new_defm()` before further use.


# defm 0.2.1.0

* Returning to CRAN.

* The `defm` now uses the same versioning as the [`barry`](https://github.com/USCbiostats/barry) library (first three numbers). The last number is the patch.


# defm 0.1-1

* Added a `NEWS.md` file to track changes to the package.

* Change how `nobs.DEFM` is documented (per CRAN)

