# ec50estimator 1.0.0

* Added `plot_EC50_curves()` for plotting raw dose-response observations and
  fitted `drc` curves across isolates, strata, and one or more candidate models.
* `estimate_EC50()` and `ec50_multimodel()` now store plotting metadata and
  fitted models, allowing `plot_EC50_curves(fit)` without repeating the formula,
  data, grouping columns, or model functions.
* Added `ec50_estimates()`, `ec50_metadata()`, `fitted_models()`, and
  `curve_data()` helpers for working directly with fitted EC50 objects.
* Added workflow helpers for model selection, fit quality, failed fits,
  prediction, reporting, data checks, and residual diagnostics.
* Modernized EC50 estimation internals with shared validation, clearer errors,
  and warnings for failed isolate-level fits.
* Reduced imported dependencies to `drc`; `ggplot2` is imported for the exported
  plotting workflow.
* Added support for `type = "relative"` and `type = "absolute"` in
  `estimate_EC50()`.
* Added automated tests for grouped estimates, argument validation, absolute
  effective-dose levels, and multimodel output.
* Converted the example dataset to standard package data format.
* Refreshed the vignette, README, and pkgdown configuration.
