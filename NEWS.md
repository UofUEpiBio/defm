# defm 0.2.2.9000 (development version)

* Requires `barry` (>= 0.2.2.9000), which fixes the `td_formula()` parser
  (USCbiostats/barry#26):
  - Covariate names containing `y<digit>` (e.g., `Day1`) were read as motif
    terms, silently changing the model: `"{y0} > {y0} x Day1"` became
    `Motif {a+}>{a+, b+} x Day1`.
  - `"{...}x Covar"` (no space before `x`) silently dropped the covariate.
  - Transitions such as `"{y0_1} > {y1}"`, with a current-time term on the
    left-hand side, were silently turned into intercept motifs; they now
    throw an error.

* Also picks up the `barry` fix for wrong draws in `sim_defm()` caused by a
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

