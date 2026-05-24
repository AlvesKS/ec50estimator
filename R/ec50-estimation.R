#' Estimate effective doses for grouped dose-response data
#'
#' @description
#' `estimate_EC50()` fits one dose-response model per isolate, optionally within
#' strata such as year, site, treatment, or fungicide. `ec50_multimodel()` repeats
#' the same workflow for several `drc` model functions and returns model-selection
#' statistics with the estimates.
#'
#' @param formula A two-sided formula identifying one numeric response and one
#'   numeric dose column, for example `growth ~ dose`.
#' @param data A data frame containing the numeric response, numeric dose,
#'   isolate, and optional stratification columns.
#' @param EC_lvl Numeric effective-dose level(s) passed to [drc::ED()]. The
#'   default estimates EC50.
#' @param isolate_col Character scalar naming the column that identifies each
#'   isolate.
#' @param strata_col Optional character vector naming columns used to split the
#'   data before fitting models.
#' @param fct A `drc` model function object such as `drc::LL.3()` for
#'   `estimate_EC50()`. For `ec50_multimodel()`, provide a list such as
#'   `list(drc::LL.3(), drc::LL.4())`.
#' @param interval Character scalar passed to [drc::ED()]. One of `"none"`,
#'   `"delta"`, `"fls"`, or `"tfls"`.
#' @param type Character scalar passed to [drc::ED()]. One of `"relative"` or
#'   `"absolute"`.
#' @param quiet Logical. If `FALSE`, failed isolate/model fits are reported with
#'   a warning.
#'
#' @return A data frame with one row per successful estimate. The first columns
#'   identify the isolate (`ID`) and strata, followed by columns returned by
#'   [drc::ED()]. `ec50_multimodel()` also appends model-selection statistics from
#'   [drc::mselect()] and a `model` column. The result keeps its data-frame
#'   behavior, but also stores the original data, formula, grouping columns, model
#'   functions, and fitted `drc` models so it can be passed directly to
#'   [plot_EC50_curves()].
#'
#' @examples
#' data(multi_isolate)
#'
#' estimate_EC50(
#'   growth ~ dose,
#'   data = multi_isolate,
#'   isolate_col = "isolate",
#'   strata_col = c("field", "fungicida"),
#'   fct = drc::LL.3()
#' )
#'
#' ec50_multimodel(
#'   growth ~ dose,
#'   data = multi_isolate,
#'   isolate_col = "isolate",
#'   strata_col = c("field", "fungicida"),
#'   fct = list(drc::LL.3(), drc::LL.4())
#' )
#'
#' @export
estimate_EC50 <- function(formula,
                          data,
                          EC_lvl = 50,
                          isolate_col,
                          strata_col = NULL,
                          fct,
                          interval = c("none", "delta", "fls", "tfls"),
                          type = c("relative", "absolute"),
                          quiet = FALSE) {
  interval <- match.arg(interval)
  type <- match.arg(type)
  validate_ec50_inputs(formula, data, EC_lvl, isolate_col, strata_col, fct)

  result <- estimate_single_model(
    formula = formula,
    data = data,
    EC_lvl = EC_lvl,
    isolate_col = isolate_col,
    strata_col = strata_col,
    fct = fct,
    interval = interval,
    type = type,
    quiet = quiet,
    include_model_stats = FALSE
  )

  new_ec50_result(
    result = result,
    formula = formula,
    data = data,
    isolate_col = isolate_col,
    strata_col = strata_col,
    fct = fct,
    result_class = "ec50_estimate"
  )
}

#' @rdname estimate_EC50
#' @export
ec50_multimodel <- function(formula,
                            data,
                            EC_lvl = 50,
                            isolate_col,
                            strata_col = NULL,
                            fct,
                            interval = c("none", "delta", "fls", "tfls"),
                            type = c("relative", "absolute"),
                            quiet = FALSE) {
  interval <- match.arg(interval)
  type <- match.arg(type)
  validate_ec50_inputs(formula, data, EC_lvl, isolate_col, strata_col, fct)

  if (!is.list(fct)) {
    stop("'fct' must be a list of drc model functions.", call. = FALSE)
  }

  results <- lapply(fct, function(model_fct) {
    estimate_single_model(
      formula = formula,
      data = data,
      EC_lvl = EC_lvl,
      isolate_col = isolate_col,
      strata_col = strata_col,
      fct = model_fct,
      interval = interval,
      type = type,
      quiet = quiet,
      include_model_stats = TRUE
    )
  })

  result <- bind_rows(results)
  attr(result, "ec50_models") <- unlist(
    lapply(results, function(single_result) attr(single_result, "ec50_models")),
    recursive = FALSE
  )
  attr(result, "ec50_quality") <- bind_rows(
    lapply(results, function(single_result) attr(single_result, "ec50_quality"))
  )
  attr(result, "ec50_failures") <- bind_rows(
    lapply(results, function(single_result) attr(single_result, "ec50_failures"))
  )

  new_ec50_result(
    result = result,
    formula = formula,
    data = data,
    isolate_col = isolate_col,
    strata_col = strata_col,
    fct = fct,
    result_class = c("ec50_multimodel", "ec50_estimate")
  )
}

validate_ec50_inputs <- function(formula,
                                 data,
                                 EC_lvl,
                                 isolate_col,
                                 strata_col,
                                 fct) {
  if (missing(formula)) {
    stop("Please specify 'formula'.", call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula, for example growth ~ dose.", call. = FALSE)
  }
  if (missing(data)) {
    stop("Missing 'data'.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  formula_vars <- formula_variables(formula)
  assert_columns(data, c(formula_vars$response, formula_vars$dose), "'formula'")
  assert_numeric_columns(data, c(formula_vars$response, formula_vars$dose), "'formula'")
  if (!is.numeric(EC_lvl) || length(EC_lvl) < 1 || any(!is.finite(EC_lvl))) {
    stop("'EC_lvl' must contain finite numeric values.", call. = FALSE)
  }
  if (missing(isolate_col)) {
    stop("Missing 'isolate_col'.", call. = FALSE)
  }
  if (!is.character(isolate_col) || length(isolate_col) != 1) {
    stop("'isolate_col' must be a character scalar.", call. = FALSE)
  }
  assert_columns(data, isolate_col, "'isolate_col'")
  if (!is.null(strata_col)) {
    if (!is.character(strata_col) || length(strata_col) < 1) {
      stop("'strata_col' must be NULL or a character vector.", call. = FALSE)
    }
    assert_columns(data, strata_col, "'strata_col'")
  }
  if (missing(fct)) {
    stop("Please specify 'fct'.", call. = FALSE)
  }

  invisible(TRUE)
}

assert_columns <- function(data, columns, argument) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      argument,
      " contains columns not found in 'data': ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

estimate_single_model <- function(formula,
                                  data,
                                  EC_lvl,
                                  isolate_col,
                                  strata_col,
                                  fct,
                                  interval,
                                  type,
                                  quiet,
                                  include_model_stats) {
  group_cols <- c(strata_col, isolate_col)
  groups <- split(seq_len(nrow(data)), data[group_cols], drop = TRUE)
  results <- vector("list", length(groups))
  failures <- character()
  fitted_models <- list()
  quality_rows <- list()
  failure_rows <- list()

  for (i in seq_along(groups)) {
    group_data <- data[groups[[i]], , drop = FALSE]
    identifiers <- group_data[1, group_cols, drop = FALSE]
    fit <- try_fit_ec50(
      formula = formula,
      data = group_data,
      EC_lvl = EC_lvl,
      fct = fct,
      interval = interval,
      type = type,
      include_model_stats = include_model_stats,
      quiet = quiet
    )

    if (inherits(fit, "try-error")) {
      failures <- c(failures, format_failure(identifiers, fit))
      failure_rows[[length(failure_rows) + 1]] <- fit_failure_row(
        identifiers = identifiers,
        isolate_col = isolate_col,
        strata_col = strata_col,
        model_name = model_label(fct),
        error = fit
      )
      next
    }

    results[[i]] <- add_identifiers(fit$estimates, identifiers, isolate_col, strata_col)
    quality_rows[[length(quality_rows) + 1]] <- fit_quality_row(
      data = group_data,
      formula = formula,
      identifiers = identifiers,
      isolate_col = isolate_col,
      strata_col = strata_col,
      model_name = fit$model_name
    )
    fitted_models[[length(fitted_models) + 1]] <- list(
      identifiers = identifiers,
      model = fit$model_name,
      fit = fit$model
    )
  }

  if (length(failures) > 0 && !quiet) {
    warning(
      "EC estimates could not be produced for ",
      length(failures),
      " group(s): ",
      paste(failures, collapse = "; "),
      call. = FALSE
    )
  }

  result <- bind_rows(results)
  if (ncol(result) == 0) {
    result <- empty_estimate_result(strata_col, include_model_stats)
  }
  attr(result, "ec50_models") <- fitted_models
  attr(result, "ec50_quality") <- bind_rows(quality_rows)
  attr(result, "ec50_failures") <- bind_rows(failure_rows)
  result
}

try_fit_ec50 <- function(formula,
                         data,
                         EC_lvl,
                         fct,
                         interval,
                         type,
                         include_model_stats,
                         quiet) {
  fit_expression <- quote({
    model <- drc::drm(formula, fct = fct, data = data)
    estimates <- as.data.frame(
      drc::ED(model, EC_lvl, interval = interval, display = FALSE, type = type)
    )
    names(estimates) <- make.names(names(estimates))
    row.names(estimates) <- NULL

    model_name <- model_label(fct)
    if (include_model_stats) {
      stats <- as.data.frame(drc::mselect(model, fctList = list(fct)))[1, , drop = FALSE]
      names(stats) <- make.names(names(stats))
      row.names(stats) <- NULL
      stats$model <- model_name
      estimates <- cbind(estimates, stats)
    }

    list(estimates = estimates, model = model, model_name = model_name)
  })
  if (quiet) {
    return(try({
      utils::capture.output(result <- suppressMessages(eval(fit_expression)))
      result
    }, silent = TRUE))
  }

  try(eval(fit_expression), silent = TRUE)
}

new_ec50_result <- function(result,
                            formula,
                            data,
                            isolate_col,
                            strata_col,
                            fct,
                            result_class) {
  fitted_models <- attr(result, "ec50_models")
  if (is.null(fitted_models)) {
    fitted_models <- list()
  }
  attr(result, "ec50_models") <- fitted_models
  if (is.null(attr(result, "ec50_quality")) || ncol(attr(result, "ec50_quality")) == 0) {
    attr(result, "ec50_quality") <- empty_fit_quality(strata_col)
  }
  if (is.null(attr(result, "ec50_failures")) || ncol(attr(result, "ec50_failures")) == 0) {
    attr(result, "ec50_failures") <- empty_fit_failures(strata_col)
  }
  attr(result, "ec50_formula") <- formula
  attr(result, "ec50_data") <- data
  attr(result, "ec50_isolate_col") <- isolate_col
  attr(result, "ec50_strata_col") <- strata_col
  attr(result, "ec50_fct") <- fct
  attr(result, "ec50_model_labels") <- unique(vapply(fitted_models, function(model) model$model, character(1)))
  class(result) <- unique(c(result_class, class(result)))
  result
}

fit_quality_row <- function(data,
                            formula,
                            identifiers,
                            isolate_col,
                            strata_col,
                            model_name) {
  vars <- formula_variables(formula)
  dose_values <- data[[vars$dose]]
  response_values <- data[[vars$response]]
  cbind(
    identifier_output(identifiers, isolate_col, strata_col),
    data.frame(
      model = model_name,
      fit_status = "ok",
      n_obs = nrow(data),
      n_doses = length(unique(dose_values[is.finite(dose_values)])),
      dose_min = suppressWarnings(min(dose_values, na.rm = TRUE)),
      dose_max = suppressWarnings(max(dose_values, na.rm = TRUE)),
      response_min = suppressWarnings(min(response_values, na.rm = TRUE)),
      response_max = suppressWarnings(max(response_values, na.rm = TRUE)),
      message = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

fit_failure_row <- function(identifiers,
                            isolate_col,
                            strata_col,
                            model_name,
                            error) {
  cbind(
    identifier_output(identifiers, isolate_col, strata_col),
    data.frame(
      model = model_name,
      message = conditionMessage(attr(error, "condition")),
      stringsAsFactors = FALSE
    )
  )
}

identifier_output <- function(identifiers, isolate_col, strata_col) {
  id <- data.frame(ID = as.character(identifiers[[isolate_col]]), stringsAsFactors = FALSE)
  strata <- identifiers[strata_col]
  if (length(strata) > 0) {
    strata <- as.data.frame(strata, stringsAsFactors = FALSE)
  }
  cbind(id, strata)
}

empty_fit_quality <- function(strata_col) {
  columns <- c(
    "ID", strata_col, "model", "fit_status", "n_obs", "n_doses",
    "dose_min", "dose_max", "response_min", "response_max", "message"
  )
  empty_data_frame(columns)
}

empty_fit_failures <- function(strata_col) {
  empty_data_frame(c("ID", strata_col, "model", "message"))
}

add_identifiers <- function(estimates, identifiers, isolate_col, strata_col) {
  id <- data.frame(ID = as.character(identifiers[[isolate_col]]), stringsAsFactors = FALSE)
  strata <- identifiers[strata_col]
  if (length(strata) > 0) {
    strata <- as.data.frame(strata, stringsAsFactors = FALSE)
  }

  cbind(id, strata, estimates)
}

format_failure <- function(identifiers, error) {
  values <- paste(
    names(identifiers),
    vapply(identifiers, as.character, character(1)),
    sep = "=",
    collapse = ", "
  )
  message <- conditionMessage(attr(error, "condition"))
  paste0("[", values, "] ", message)
}

model_label <- function(fct) {
  if (!is.null(fct$text)) {
    label <- model_name_from_drc_text(as.character(fct$text))
    if (!is.na(label)) {
      return(label)
    }
    return(as.character(fct$text))
  }
  if (!is.null(fct$name)) {
    return(as.character(fct$name))
  }
  class(fct)[1]
}

model_name_from_drc_text <- function(model_text) {
  functions <- try(drc::getMeanFunctions(display = FALSE), silent = TRUE)
  if (inherits(functions, "try-error")) {
    return(NA_character_)
  }

  functions <- as.data.frame(functions, stringsAsFactors = FALSE)
  names(functions) <- as.character(functions[1, ])
  functions <- functions[-1, , drop = FALSE]
  matches <- which(functions == model_text, arr.ind = TRUE)
  if (nrow(matches) == 0) {
    return(NA_character_)
  }

  names(functions)[matches[1, "col"]]
}

bind_rows <- function(dfs) {
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) {
    return(data.frame())
  }

  all_names <- unique(unlist(lapply(dfs, names), use.names = FALSE))
  dfs <- lapply(dfs, function(df) {
    missing_names <- setdiff(all_names, names(df))
    for (name in missing_names) {
      df[[name]] <- NA
    }
    df[all_names]
  })

  do.call(rbind, dfs)
}

empty_data_frame <- function(columns) {
  result <- stats::setNames(rep(list(character()), length(columns)), columns)
  as.data.frame(result, stringsAsFactors = FALSE)
}

assert_numeric_columns <- function(data, columns, argument) {
  non_numeric <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      argument,
      " must identify numeric response and dose columns. Non-numeric column(s): ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }
}

empty_estimate_result <- function(strata_col, include_model_stats) {
  columns <- c("ID", strata_col, "Estimate", "Std..Error")
  if (include_model_stats) {
    columns <- c(columns, "logLik", "IC", "Lack.of.fit", "Res.var", "model")
  }
  empty_data_frame(columns)
}
