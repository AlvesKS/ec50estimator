#' Extract information from fitted EC50 objects
#'
#' @description
#' These helpers make fitted objects from [estimate_EC50()] and
#' [ec50_multimodel()] easier to reuse. They return plain data frames, metadata,
#' stored `drc` models, or fitted curve coordinates without requiring users to
#' work with object attributes directly.
#'
#' @param x An object returned by [estimate_EC50()] or [ec50_multimodel()].
#' @param n_points Number of dose values used to draw each fitted curve.
#' @param log_x Logical. If `TRUE`, curve coordinates are generated only over
#'   positive dose values for log10 x-axis plotting.
#' @param quiet Logical. If `FALSE`, failed curve predictions are reported with a
#'   warning.
#'
#' @return
#' `ec50_estimates()` returns a plain data frame of EC estimates.
#' `ec50_metadata()` returns a list with modeling metadata.
#' `fitted_models()` returns a named list of stored `drc` model objects.
#' `curve_data()` returns a data frame of fitted curve coordinates.
#'
#' @examples
#' data(multi_isolate)
#' sample_data <- subset(
#'   multi_isolate,
#'   isolate %in% 1:3 & fungicida == "Fungicide A"
#' )
#'
#' fit <- estimate_EC50(
#'   growth ~ dose,
#'   data = sample_data,
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = drc::LL.3()
#' )
#'
#' ec50_estimates(fit)
#' ec50_metadata(fit)
#' curve_data(fit)
#'
#' @name ec50_extractors
NULL

#' @rdname ec50_extractors
#' @export
ec50_estimates <- function(x) {
  validate_ec50_fit_object(x)
  result <- as.data.frame(x, stringsAsFactors = FALSE)
  strip_ec50_attributes(result)
}

#' @rdname ec50_extractors
#' @export
ec50_metadata <- function(x) {
  validate_ec50_fit_object(x)
  list(
    formula = attr(x, "ec50_formula"),
    data_columns = names(attr(x, "ec50_data")),
    isolate_col = attr(x, "ec50_isolate_col"),
    strata_col = attr(x, "ec50_strata_col"),
    model_labels = attr(x, "ec50_model_labels"),
    n_models = length(attr(x, "ec50_models"))
  )
}

#' @rdname ec50_extractors
#' @export
fitted_models <- function(x) {
  validate_ec50_fit_object(x)
  models <- attr(x, "ec50_models")
  model_fits <- lapply(models, `[[`, "fit")
  names(model_fits) <- fitted_model_names(models)
  model_fits
}

#' @rdname ec50_extractors
#' @export
curve_data <- function(x, n_points = 200, log_x = TRUE, quiet = FALSE) {
  curve_data_from_fit(
    x = x,
    n_points = n_points,
    log_x = log_x,
    quiet = quiet,
    plot_cols = character(0)
  )
}

curve_data_from_fit <- function(x,
                                n_points,
                                log_x,
                                quiet,
                                plot_cols) {
  validate_ec50_fit_object(x)
  validate_curve_data_inputs(n_points, log_x)

  formula <- attr(x, "ec50_formula")
  data <- attr(x, "ec50_data")
  isolate_col <- attr(x, "ec50_isolate_col")
  strata_col <- attr(x, "ec50_strata_col")
  fitted <- attr(x, "ec50_models")
  if (length(fitted) == 0) {
    stop("'x' does not contain stored fitted models.", call. = FALSE)
  }

  vars <- formula_variables(formula)
  build_curve_predictions_from_fits(
    formula = formula,
    data = data,
    isolate_col = isolate_col,
    strata_col = strata_col,
    plot_cols = character(0),
    dose_col = vars$dose,
    fitted_models = fitted,
    n_points = n_points,
    log_x = log_x,
    quiet = quiet
  )
}

validate_ec50_fit_object <- function(x) {
  if (!inherits(x, "ec50_estimate")) {
    stop(
      "'x' must be an object returned by estimate_EC50() or ec50_multimodel().",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_curve_data_inputs <- function(n_points, log_x) {
  if (!is.numeric(n_points) || length(n_points) != 1 || !is.finite(n_points) || n_points < 2) {
    stop("'n_points' must be a finite number greater than or equal to 2.", call. = FALSE)
  }
  if (!is.logical(log_x) || length(log_x) != 1 || is.na(log_x)) {
    stop("'log_x' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

strip_ec50_attributes <- function(x) {
  attributes(x) <- attributes(x)[c("names", "row.names")]
  class(x) <- "data.frame"
  x
}

fitted_model_names <- function(models) {
  if (length(models) == 0) {
    return(character(0))
  }

  vapply(models, function(model) {
    identifiers <- model$identifiers
    id_values <- paste(
      names(identifiers),
      vapply(identifiers, as.character, character(1)),
      sep = "=",
      collapse = "_"
    )
    paste(id_values, model$model, sep = "_model=")
  }, character(1))
}
