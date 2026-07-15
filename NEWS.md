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

