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
* Reorganized pkgdown and vignettes around the user workflow: check data, fit
  models, diagnose fits, select models, plot curves, predict, and report.
* Added a new getting-started vignette and rewrote the single-model and
  multimodel vignettes to emphasize fit objects as the main interface.
* Removed row-name warnings from `predict_ec50()` and `residual_data()`.
* Added clearer validation for formula, response, and dose inputs, including
  stable empty outputs when no groups can be fitted.
* Reset row names in extractor outputs and preserved model-selection columns for
  all-failed multimodel fits.
* Made `quiet = TRUE` suppress informational messages from internal `drc`
  fitting calls.
* Added a clear error when `plot_EC50_curves()` is asked for `models = "best"`
  without a fitted `ec50_multimodel` object.
* Modernized EC50 estimation internals with shared validation, clearer errors,
  and warnings for failed isolate-level fits.
* Kept runtime dependencies focused on `drc` and `ggplot2`.
* Removed vignette-only dependencies on `cowplot` and `ggridges`.
* Added support for `type = "relative"` and `type = "absolute"` in
  `estimate_EC50()`.
* Added automated tests for grouped estimates, argument validation, absolute
  effective-dose levels, and multimodel output.
* Converted the example dataset to standard package data format.
* Refreshed the vignette, README, and pkgdown configuration.
