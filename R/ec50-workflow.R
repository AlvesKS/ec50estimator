#' Select EC50 candidate models
#'
#' @description
#' Rank candidate models fitted with [ec50_multimodel()] within each isolate and
#' stratum using an information criterion such as `IC`.
#'
#' @param x An object returned by [ec50_multimodel()].
#' @param criterion Character scalar naming the criterion column. Smaller values
#'   are considered better.
#'
#' @return A data frame with model rankings, delta criterion values, and weights.
#'
#' @examples
#' data(multi_isolate)
#' sample_data <- subset(multi_isolate, isolate %in% 1:3 & fungicida == "Fungicide A")
#' fit <- ec50_multimodel(
#'   growth ~ dose,
#'   data = sample_data,
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = list(drc::LL.3(), drc::LL.4())
#' )
#' model_selection(fit)
#' best_model(fit)
#'
#' @export
model_selection <- function(x, criterion = "IC") {
  validate_multimodel_object(x)
  if (!is.character(criterion) || length(criterion) != 1) {
    stop("'criterion' must be a character scalar.", call. = FALSE)
  }

  estimates <- ec50_estimates(x)
  required <- c("ID", "model", criterion)
  assert_columns(estimates, required, "'x'")
  if (nrow(estimates) == 0) {
    estimates$delta <- numeric()
    estimates$weight <- numeric()
    estimates$rank <- integer()
    return(estimates)
  }
  group_cols <- c("ID", attr(x, "ec50_strata_col"))
  groups <- split(seq_len(nrow(estimates)), estimates[group_cols], drop = TRUE)

  ranked <- lapply(groups, function(rows) {
    group_data <- estimates[rows, , drop = FALSE]
    criterion_values <- group_data[[criterion]]
    order_index <- order(criterion_values, na.last = TRUE)
    group_data <- group_data[order_index, , drop = FALSE]
    criterion_values <- group_data[[criterion]]
    best_value <- suppressWarnings(min(criterion_values, na.rm = TRUE))
    if (!is.finite(best_value)) {
      group_data$delta <- NA_real_
      group_data$weight <- NA_real_
    } else {
      group_data$delta <- criterion_values - best_value
      relative_likelihood <- exp(-0.5 * group_data$delta)
      group_data$weight <- relative_likelihood / sum(relative_likelihood, na.rm = TRUE)
    }
    group_data$rank <- seq_len(nrow(group_data))
    group_data
  })

  reset_row_names(bind_rows(ranked))
}

#' @rdname model_selection
#' @export
best_model <- function(x, criterion = "IC") {
  ranked <- model_selection(x, criterion = criterion)
  best <- ranked[ranked$rank == 1, , drop = FALSE]
  row.names(best) <- NULL
  best
}

#' Inspect EC50 fit quality and failures
#'
#' @description
#' `fit_quality()` returns group-level quality information for successful fits.
#' `fit_failures()` returns failed group/model combinations as data.
#'
#' @param x An object returned by [estimate_EC50()] or [ec50_multimodel()].
#'
#' @return A data frame.
#'
#' @examples
#' data(multi_isolate)
#' fit <- estimate_EC50(
#'   growth ~ dose,
#'   data = subset(multi_isolate, isolate %in% 1:3 & fungicida == "Fungicide A"),
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = drc::LL.3()
#' )
#' fit_quality(fit)
#' fit_failures(fit)
#'
#' @export
fit_quality <- function(x) {
  validate_ec50_fit_object(x)
  quality <- attr(x, "ec50_quality")
  if (is.null(quality)) {
    quality <- empty_fit_quality(attr(x, "ec50_strata_col"))
  }
  strip_ec50_attributes(quality)
}

#' @rdname fit_quality
#' @export
fit_failures <- function(x) {
  validate_ec50_fit_object(x)
  failures <- attr(x, "ec50_failures")
  if (is.null(failures)) {
    failures <- empty_fit_failures(attr(x, "ec50_strata_col"))
  }
  strip_ec50_attributes(failures)
}

#' Predict responses from fitted EC50 models
#'
#' @description
#' Predict response values at user-supplied doses from stored `drc` model fits.
#'
#' @param x An object returned by [estimate_EC50()] or [ec50_multimodel()].
#' @param dose Numeric vector of dose values for prediction.
#' @param models One of `"all"`, `"best"`, or a character vector of model names.
#'
#' @return A data frame with group identifiers, model, dose, and predicted value.
#'
#' @examples
#' data(multi_isolate)
#' fit <- estimate_EC50(
#'   growth ~ dose,
#'   data = subset(multi_isolate, isolate == 1 & fungicida == "Fungicide A"),
#'   isolate_col = "isolate",
#'   fct = drc::LL.3()
#' )
#' predict_ec50(fit, dose = c(0.001, 0.01, 0.1))
#'
#' @export
predict_ec50 <- function(x, dose, models = "all") {
  validate_ec50_fit_object(x)
  if (missing(dose) || !is.numeric(dose) || length(dose) < 1 || any(!is.finite(dose))) {
    stop("'dose' must contain finite numeric values.", call. = FALSE)
  }

  records <- select_fitted_model_records(x, models = models)
  formula <- attr(x, "ec50_formula")
  vars <- formula_variables(formula)
  predictions <- lapply(records, function(record) {
    newdata <- data.frame(dose)
    names(newdata) <- vars$dose
    data.frame(
      repeated_identifier_output(x, record, nrow(newdata)),
      model = record$model,
      newdata,
      predicted = as.numeric(stats::predict(record$fit, newdata = newdata)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  reset_row_names(bind_rows(predictions))
}

#' Build EC50 report tables
#'
#' @description
#' Return a plain data frame suitable for reporting estimates from all, best, or
#' selected models.
#'
#' @param x An object returned by [estimate_EC50()] or [ec50_multimodel()].
#' @param models One of `"all"`, `"best"`, or a character vector of model names.
#'
#' @return A plain data frame.
#'
#' @examples
#' data(multi_isolate)
#' fit <- estimate_EC50(
#'   growth ~ dose,
#'   data = subset(multi_isolate, isolate %in% 1:3 & fungicida == "Fungicide A"),
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = drc::LL.3()
#' )
#' report_ec50(fit)
#'
#' @export
report_ec50 <- function(x, models = "all") {
  validate_ec50_fit_object(x)
  estimates <- ec50_estimates(x)
  selected <- selected_model_table(x, models = models)
  if (is.null(selected)) {
    return(estimates)
  }

  keys <- model_keys(selected, attr(x, "ec50_strata_col"))
  estimates[model_keys(estimates, attr(x, "ec50_strata_col")) %in% keys, , drop = FALSE]
}

#' Check dose-response data before EC50 fitting
#'
#' @description
#' Run group-level checks for common dose-response data problems before fitting
#' EC50 models.
#'
#' @param data A data frame.
#' @param response,dose,isolate Character scalars naming the response, dose, and
#'   isolate columns.
#' @param strata Optional character vector of grouping columns.
#' @param log_x Logical. If `TRUE`, nonpositive dose values are flagged.
#'
#' @return A data frame with one row per isolate/strata group.
#'
#' @examples
#' data(multi_isolate)
#' check_ec50_data(
#'   multi_isolate,
#'   response = "growth",
#'   dose = "dose",
#'   isolate = "isolate",
#'   strata = c("field", "fungicida")
#' )
#'
#' @export
check_ec50_data <- function(data,
                            response,
                            dose,
                            isolate,
                            strata = NULL,
                            log_x = TRUE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  for (arg in list(response = response, dose = dose, isolate = isolate)) {
    arg_name <- names(arg)
    arg_value <- arg[[1]]
    if (!is.character(arg_value) || length(arg_value) != 1) {
      stop("'", arg_name, "' must be a character scalar.", call. = FALSE)
    }
  }
  if (!is.null(strata) && (!is.character(strata) || length(strata) < 1)) {
    stop("'strata' must be NULL or a character vector.", call. = FALSE)
  }
  assert_columns(data, c(response, dose, isolate, strata), "'data'")
  assert_numeric_columns(data, c(response, dose), "'data'")
  if (!is.logical(log_x) || length(log_x) != 1 || is.na(log_x)) {
    stop("'log_x' must be TRUE or FALSE.", call. = FALSE)
  }

  group_cols <- c(strata, isolate)
  if (nrow(data) == 0) {
    return(empty_check_ec50_data(strata))
  }
  groups <- split(seq_len(nrow(data)), data[group_cols], drop = TRUE)
  checks <- lapply(groups, function(rows) {
    group_data <- data[rows, , drop = FALSE]
    dose_values <- group_data[[dose]]
    response_values <- group_data[[response]]
    duplicated_rows <- duplicated(group_data[c(dose, response)])
    identifiers <- group_data[1, group_cols, drop = FALSE]
    cbind(
      identifier_output(identifiers, isolate, strata),
      data.frame(
        n_obs = nrow(group_data),
        n_doses = length(unique(dose_values[is.finite(dose_values)])),
        missing_response = sum(is.na(response_values)),
        missing_dose = sum(is.na(dose_values)),
        nonpositive_dose = if (log_x) sum(!is.na(dose_values) & dose_values <= 0) else 0,
        duplicated_rows = sum(duplicated_rows),
        no_response_variation = length(unique(response_values[!is.na(response_values)])) < 2,
        too_few_observations = nrow(group_data) < 4,
        too_few_doses = length(unique(dose_values[is.finite(dose_values)])) < 3,
        stringsAsFactors = FALSE
      )
    )
  })

  reset_row_names(bind_rows(checks))
}

#' Extract and plot EC50 residual diagnostics
#'
#' @description
#' `residual_data()` returns observed, fitted, and residual values from stored
#' models. `plot_residuals()` returns a `ggplot2` diagnostic plot.
#'
#' @param x An object returned by [estimate_EC50()] or [ec50_multimodel()].
#' @param models One of `"all"`, `"best"`, or a character vector of model names.
#' @param type For `plot_residuals()`, plot residuals against fitted values or
#'   dose.
#'
#' @return `residual_data()` returns a data frame; `plot_residuals()` returns a
#'   `ggplot2` object.
#'
#' @examples
#' data(multi_isolate)
#' fit <- estimate_EC50(
#'   growth ~ dose,
#'   data = subset(multi_isolate, isolate == 1 & fungicida == "Fungicide A"),
#'   isolate_col = "isolate",
#'   fct = drc::LL.3()
#' )
#' residual_data(fit)
#' plot_residuals(fit)
#'
#' @export
residual_data <- function(x, models = "all") {
  validate_ec50_fit_object(x)
  records <- select_fitted_model_records(x, models = models)
  data <- attr(x, "ec50_data")
  vars <- formula_variables(attr(x, "ec50_formula"))

  residuals <- lapply(records, function(record) {
    group_data <- data[rows_matching_identifiers(data, record$identifiers), , drop = FALSE]
    newdata <- group_data[vars$dose]
    fitted <- as.numeric(stats::predict(record$fit, newdata = newdata))
    observed <- group_data[[vars$response]]
    data.frame(
      repeated_identifier_output(x, record, nrow(group_data)),
      model = record$model,
      dose = group_data[[vars$dose]],
      observed = observed,
      fitted = fitted,
      residual = observed - fitted,
      stringsAsFactors = FALSE
    )
  })

  reset_row_names(bind_rows(residuals))
}

repeated_identifier_output <- function(x, record, n) {
  identifiers <- identifier_output(
    record$identifiers,
    attr(x, "ec50_isolate_col"),
    attr(x, "ec50_strata_col")
  )
  identifiers <- identifiers[rep(1, n), , drop = FALSE]
  row.names(identifiers) <- NULL
  identifiers
}

#' @rdname residual_data
#' @export
plot_residuals <- function(x, models = "all", type = c("fitted", "dose")) {
  type <- match.arg(type)
  data <- residual_data(x, models = models)
  x_col <- if (type == "fitted") "fitted" else "dose"
  mapping <- do.call(
    ggplot2::aes,
    list(x = as.name(x_col), y = as.name("residual"), color = as.name("model"))
  )
  plot <- ggplot2::ggplot(data, mapping) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::labs(x = x_col, y = "Residual", color = "Model") +
    ggplot2::theme_light()
  if (type == "dose" && all(data$dose > 0)) {
    plot <- plot + ggplot2::scale_x_log10()
  }
  add_curve_facets(plot, attr(x, "ec50_strata_col"), NULL, NULL)
}

validate_multimodel_object <- function(x) {
  validate_ec50_fit_object(x)
  if (!inherits(x, "ec50_multimodel")) {
    stop("'x' must be an object returned by ec50_multimodel().", call. = FALSE)
  }
  invisible(TRUE)
}

select_fitted_model_records <- function(x, models = "all") {
  validate_ec50_fit_object(x)
  fitted <- attr(x, "ec50_models")
  if (length(fitted) == 0) {
    stop("'x' does not contain stored fitted models.", call. = FALSE)
  }
  selected <- selected_model_table(x, models = models)
  if (is.null(selected)) {
    return(fitted)
  }

  keys <- model_keys(selected, attr(x, "ec50_strata_col"))
  fitted[vapply(fitted, function(record) {
    record_key(record, attr(x, "ec50_isolate_col"), attr(x, "ec50_strata_col")) %in% keys
  }, logical(1))]
}

selected_model_table <- function(x, models = "all") {
  if (!is.character(models) || length(models) < 1) {
    stop("'models' must be 'all', 'best', or a character vector of model names.", call. = FALSE)
  }
  if (length(models) == 1 && models == "all") {
    return(NULL)
  }
  if (length(models) == 1 && models == "best") {
    if (inherits(x, "ec50_multimodel")) {
      return(best_model(x))
    }
    return(NULL)
  }

  estimates <- ec50_estimates(x)
  if (!"model" %in% names(estimates)) {
    stop("Model names are not available for this fitted object.", call. = FALSE)
  }
  selected <- estimates[estimates$model %in% models, , drop = FALSE]
  if (nrow(selected) == 0) {
    stop("No fitted models matched 'models'.", call. = FALSE)
  }
  selected
}

model_keys <- function(data, strata_col) {
  key_cols <- c("ID", strata_col, "model")
  do.call(paste, c(data[key_cols], sep = "\r"))
}

record_key <- function(record, isolate_col, strata_col) {
  data <- cbind(
    identifier_output(record$identifiers, isolate_col, strata_col),
    data.frame(model = record$model, stringsAsFactors = FALSE)
  )
  model_keys(data, strata_col)
}

reset_row_names <- function(data) {
  row.names(data) <- NULL
  data
}

empty_check_ec50_data <- function(strata) {
  columns <- c(
    "ID", strata, "n_obs", "n_doses", "missing_response", "missing_dose",
    "nonpositive_dose", "duplicated_rows", "no_response_variation",
    "too_few_observations", "too_few_doses"
  )
  empty_data_frame(columns)
}
