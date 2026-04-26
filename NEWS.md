# ec50estimator 1.0.0

* Modernized EC50 estimation internals with shared validation, clearer errors,
  and warnings for failed isolate-level fits.
* Reduced imported dependencies to `drc`.
* Added support for `type = "relative"` and `type = "absolute"` in
  `estimate_EC50()`.
* Added automated tests for grouped estimates, argument validation, absolute
  effective-dose levels, and multimodel output.
* Converted the example dataset to standard package data format.
* Refreshed the vignette, README, and pkgdown configuration.
