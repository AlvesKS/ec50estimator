#' Plot fitted dose-response curves for multiple isolates
#'
#' @description
#' `plot_EC50_curves()` fits one or more `drc` dose-response models for each
#' isolate and optional stratum, then returns a `ggplot2` object with raw
#' observations and fitted curves.
#'
#' @param formula A two-sided formula passed to [drc::drm()], for example
#'   `growth ~ dose`. The plotting helper supports exactly one predictor on the
#'   right-hand side.
#' @param data A data frame containing the response, dose, isolate, and optional
#'   stratification columns.
#' @param isolate_col Character scalar naming the column that identifies each
#'   isolate.
#' @param strata_col Optional character vector naming columns used to split the
#'   data before fitting models.
#' @param fct A `drc` model function object such as `drc::LL.3()`, or a list of
#'   model function objects such as `list(drc::LL.3(), drc::LL.4())`.
#' @param color_col Character scalar naming the column mapped to curve and point
#'   color. Defaults to `isolate_col`.
#' @param facet_col,facet_row Optional character scalars naming columns used for
#'   faceting. When omitted, the first two `strata_col` values are used.
#' @param n_points Number of dose values used to draw each fitted curve.
#' @param log_x Logical. If `TRUE`, use a log10 x-axis and omit non-positive dose
#'   values from the plotted data and prediction grid.
#' @param point_size,point_alpha Size and alpha for raw observation points.
#' @param line_width Width for fitted curves.
#' @param quiet Logical. If `FALSE`, failed group/model fits are reported with a
#'   warning.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' data(multi_isolate)
#' sample_data <- subset(
#'   multi_isolate,
#'   isolate %in% 1:4 & fungicida == "Fungicide A"
#' )
#'
#' plot_EC50_curves(
#'   growth ~ dose,
#'   data = sample_data,
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = drc::LL.3()
#' )
#'
#' plot_EC50_curves(
#'   growth ~ dose,
#'   data = sample_data,
#'   isolate_col = "isolate",
#'   strata_col = "field",
#'   fct = list(drc::LL.3(), drc::LL.4())
#' )
#'
#' @export
plot_EC50_curves <- function(formula,
                             data,
                             isolate_col,
                             strata_col = NULL,
                             fct,
                             color_col = isolate_col,
                             facet_col = NULL,
                             facet_row = NULL,
                             n_points = 200,
                             log_x = TRUE,
                             point_size = 2,
                             point_alpha = 0.8,
                             line_width = 1,
                             quiet = FALSE) {
  validate_ec50_inputs(
    formula = formula,
    data = data,
    EC_lvl = 50,
    isolate_col = isolate_col,
    strata_col = strata_col,
    fct = fct
  )
  validate_plot_inputs(
    formula = formula,
    data = data,
    color_col = color_col,
    facet_col = facet_col,
    facet_row = facet_row,
    n_points = n_points,
    log_x = log_x,
    point_size = point_size,
    point_alpha = point_alpha,
    line_width = line_width
  )

  vars <- formula_variables(formula)
  response_col <- vars$response
  dose_col <- vars$dose
  model_list <- normalize_model_list(fct)
  prediction_data <- build_curve_predictions(
    formula = formula,
    data = data,
    isolate_col = isolate_col,
    strata_col = strata_col,
    plot_cols = unique(c(color_col, facet_col, facet_row)),
    dose_col = dose_col,
    model_list = model_list,
    n_points = n_points,
    log_x = log_x,
    quiet = quiet
  )
  if (nrow(prediction_data) == 0) {
    stop("No fitted curves could be produced.", call. = FALSE)
  }
  observed_data <- data
  if (log_x) {
    observed_data <- observed_data[is.finite(observed_data[[dose_col]]) & observed_data[[dose_col]] > 0, , drop = FALSE]
  }

  point_mapping <- do.call(
    ggplot2::aes,
    list(x = as.name(dose_col), y = as.name(response_col), color = as.name(color_col))
  )
  line_mapping <- do.call(
    ggplot2::aes,
    list(
      x = as.name(dose_col),
      y = as.name(response_col),
      color = as.name(color_col),
      group = as.name(".curve_group")
    )
  )
  if (length(model_list) > 1) {
    line_mapping$linetype <- as.name("model")
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = observed_data,
      mapping = point_mapping,
      size = point_size,
      alpha = point_alpha
    ) +
    ggplot2::geom_line(
      data = prediction_data,
      mapping = line_mapping,
      linewidth = line_width
    ) +
    ggplot2::labs(x = dose_col, y = response_col, color = color_col) +
    ggplot2::theme_light()

  if (length(model_list) > 1) {
    plot <- plot + ggplot2::labs(linetype = "model")
  }
  if (log_x) {
    plot <- plot + ggplot2::scale_x_log10()
  }

  add_curve_facets(plot, strata_col, facet_col, facet_row)
}

validate_plot_inputs <- function(formula,
                                 data,
                                 color_col,
                                 facet_col,
                                 facet_row,
                                 n_points,
                                 log_x,
                                 point_size,
                                 point_alpha,
                                 line_width) {
  formula_vars <- formula_variables(formula)
  assert_columns(data, c(formula_vars$response, formula_vars$dose), "'formula'")
  if (!is.character(color_col) || length(color_col) != 1) {
    stop("'color_col' must be a character scalar.", call. = FALSE)
  }
  assert_columns(data, color_col, "'color_col'")
  facet_args <- list(facet_col = facet_col, facet_row = facet_row)
  for (facet_name in names(facet_args)) {
    facet_value <- facet_args[[facet_name]]
    if (!is.null(facet_value)) {
      if (!is.character(facet_value) || length(facet_value) != 1) {
        stop("'", facet_name, "' must be NULL or a character scalar.", call. = FALSE)
      }
      assert_columns(data, facet_value, paste0("'", facet_name, "'"))
    }
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || !is.finite(n_points) || n_points < 2) {
    stop("'n_points' must be a finite number greater than or equal to 2.", call. = FALSE)
  }
  if (!is.logical(log_x) || length(log_x) != 1 || is.na(log_x)) {
    stop("'log_x' must be TRUE or FALSE.", call. = FALSE)
  }
  for (numeric_arg in list(point_size = point_size, point_alpha = point_alpha, line_width = line_width)) {
    arg_name <- names(numeric_arg)
    arg_value <- numeric_arg[[1]]
    if (!is.numeric(arg_value) || length(arg_value) != 1 || !is.finite(arg_value) || arg_value < 0) {
      stop("'", arg_name, "' must be a non-negative finite number.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

formula_variables <- function(formula) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    stop(
      "'formula' must be a two-sided formula, for example growth ~ dose.",
      call. = FALSE
    )
  }
  response <- all.vars(formula[[2]])
  predictors <- all.vars(formula[[3]])
  if (length(response) != 1 || length(predictors) != 1) {
    stop(
      "'formula' must contain one response and one predictor, for example growth ~ dose.",
      call. = FALSE
    )
  }

  list(response = response, dose = predictors)
}

normalize_model_list <- function(fct) {
  if (is.list(fct) && is.null(fct$name) && is.null(fct$text)) {
    return(fct)
  }

  list(fct)
}

build_curve_predictions <- function(formula,
                                    data,
                                    isolate_col,
                                    strata_col,
                                    plot_cols,
                                    dose_col,
                                    model_list,
                                    n_points,
                                    log_x,
                                    quiet) {
  group_cols <- c(strata_col, isolate_col)
  groups <- split(seq_len(nrow(data)), data[group_cols], drop = TRUE)
  prediction_results <- list()
  failures <- character()

  for (i in seq_along(groups)) {
    group_data <- data[groups[[i]], , drop = FALSE]
    identifier_cols <- unique(c(group_cols, plot_cols))
    identifiers <- group_data[1, identifier_cols, drop = FALSE]

    for (j in seq_along(model_list)) {
      model_fct <- model_list[[j]]
      model_name <- model_label(model_fct)
      prediction <- try_predict_curve(
        formula = formula,
        data = group_data,
        identifiers = identifiers,
        dose_col = dose_col,
        model_fct = model_fct,
        model_name = model_name,
        n_points = n_points,
        log_x = log_x
      )

      if (inherits(prediction, "try-error")) {
        failures <- c(failures, format_failure(identifiers, prediction))
        next
      }

      prediction_results[[length(prediction_results) + 1]] <- prediction
    }
  }

  if (length(failures) > 0 && !quiet) {
    warning(
      "Fitted curves could not be produced for ",
      length(failures),
      " group/model combination(s): ",
      paste(failures, collapse = "; "),
      call. = FALSE
    )
  }

  bind_rows(prediction_results)
}

try_predict_curve <- function(formula,
                              data,
                              identifiers,
                              dose_col,
                              model_fct,
                              model_name,
                              n_points,
                              log_x) {
  try({
    model_data <- data
    if (log_x) {
      model_data <- model_data[is.finite(model_data[[dose_col]]) & model_data[[dose_col]] > 0, , drop = FALSE]
    }
    if (nrow(model_data) == 0) {
      stop("no positive dose values available for log-scale plotting", call. = FALSE)
    }

    dose_values <- model_data[[dose_col]]
    dose_grid <- make_dose_grid(dose_values, n_points, log_x)
    newdata <- data.frame(dose_grid)
    names(newdata) <- dose_col
    model <- drc::drm(formula, fct = model_fct, data = model_data)
    predicted <- stats::predict(model, newdata = newdata)

    prediction <- data.frame(
      identifiers[rep(1, length(dose_grid)), , drop = FALSE],
      model = model_name,
      newdata,
      .predicted = as.numeric(predicted),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    vars <- formula_variables(formula)
    names(prediction)[names(prediction) == ".predicted"] <- vars$response
    prediction$.curve_group <- interaction(
      prediction[c(names(identifiers), "model")],
      drop = TRUE,
      lex.order = TRUE
    )
    prediction
  }, silent = TRUE)
}

make_dose_grid <- function(dose_values, n_points, log_x) {
  dose_values <- dose_values[is.finite(dose_values)]
  if (log_x) {
    dose_values <- dose_values[dose_values > 0]
    return(10^seq(log10(min(dose_values)), log10(max(dose_values)), length.out = n_points))
  }

  seq(min(dose_values), max(dose_values), length.out = n_points)
}

add_curve_facets <- function(plot, strata_col, facet_col, facet_row) {
  if (is.null(facet_col) && length(strata_col) >= 1) {
    facet_col <- strata_col[1]
  }
  if (is.null(facet_row) && length(strata_col) >= 2) {
    facet_row <- strata_col[2]
  }

  if (!is.null(facet_col) && !is.null(facet_row)) {
    return(plot + ggplot2::facet_grid(stats::as.formula(paste(facet_row, "~", facet_col))))
  }
  if (!is.null(facet_col)) {
    return(plot + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col))))
  }
  if (!is.null(facet_row)) {
    return(plot + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_row))))
  }

  plot
}
