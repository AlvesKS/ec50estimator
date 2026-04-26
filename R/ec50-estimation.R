#' Estimate effective doses for grouped dose-response data
#'
#' @description
#' `estimate_EC50()` fits one dose-response model per isolate, optionally within
#' strata such as year, site, treatment, or fungicide. `ec50_multimodel()` repeats
#' the same workflow for several `drc` model functions and returns model-selection
#' statistics with the estimates.
#'
#' @param formula A two-sided formula passed to [drc::drm()], for example
#'   `growth ~ dose`.
#' @param data A data frame containing the response, dose, isolate, and optional
#'   stratification columns.
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
#'   [drc::mselect()] and a `model` column.
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

  estimate_single_model(
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

  bind_rows(results)
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
      include_model_stats = include_model_stats
    )

    if (inherits(fit, "try-error")) {
      failures <- c(failures, format_failure(identifiers, fit))
      next
    }

    results[[i]] <- add_identifiers(fit, identifiers, isolate_col, strata_col)
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

  bind_rows(results)
}

try_fit_ec50 <- function(formula,
                         data,
                         EC_lvl,
                         fct,
                         interval,
                         type,
                         include_model_stats) {
  try({
    model <- drc::drm(formula, fct = fct, data = data)
    estimates <- as.data.frame(
      drc::ED(model, EC_lvl, interval = interval, display = FALSE, type = type)
    )
    names(estimates) <- make.names(names(estimates))
    row.names(estimates) <- NULL

    if (include_model_stats) {
      stats <- as.data.frame(drc::mselect(model, fctList = list(fct)))[1, , drop = FALSE]
      names(stats) <- make.names(names(stats))
      row.names(stats) <- NULL
      stats$model <- model_label(fct)
      estimates <- cbind(estimates, stats)
    }

    estimates
  }, silent = TRUE)
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
