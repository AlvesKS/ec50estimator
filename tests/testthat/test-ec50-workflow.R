test_that("model_selection ranks models within each group", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3) & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    strata_col = "field",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  selection <- model_selection(fit)
  groups <- split(seq_len(nrow(selection)), selection[c("ID", "field")], drop = TRUE)

  expect_s3_class(selection, "data.frame")
  expect_true(all(c("ID", "field", "model", "IC", "delta", "weight", "rank") %in% names(selection)))
  expect_true(all(vapply(groups, function(rows) min(selection$delta[rows]) == 0, logical(1))))
  expect_true(all(abs(vapply(groups, function(rows) sum(selection$weight[rows]), numeric(1)) - 1) < 1e-8))
  expect_equal(nrow(best_model(fit)), length(groups))
})

test_that("model_selection validates multimodel objects", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  expect_error(model_selection(fit), "ec50_multimodel")
  expect_error(best_model(fit), "ec50_multimodel")
})

test_that("model_selection preserves columns when all multimodel fits fail", {
  data(multi_isolate)
  bad_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )[1, , drop = FALSE]

  expect_silent(
    fit <- ec50_multimodel(
      growth ~ dose,
      data = bad_data,
      isolate_col = "isolate",
      strata_col = "field",
      fct = list(drc::LL.3(), drc::LL.4()),
      quiet = TRUE
    )
  )

  selection <- model_selection(fit)
  best <- best_model(fit)

  expect_equal(nrow(selection), 0)
  expect_true(all(c("ID", "field", "model", "IC", "delta", "weight", "rank") %in% names(selection)))
  expect_named(best, names(selection))
})

test_that("fit_quality and fit_failures expose successful and failed fits", {
  data(multi_isolate)
  good_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )
  bad_data <- good_data[1, , drop = FALSE]
  bad_data$isolate <- 999
  sample_data <- rbind(good_data, bad_data)

  expect_warning(
    fit <- estimate_EC50(
      growth ~ dose,
      data = sample_data,
      isolate_col = "isolate",
      fct = drc::LL.3()
    ),
    "could not be produced"
  )

  quality <- fit_quality(fit)
  failures <- fit_failures(fit)

  expect_true(all(c("ID", "model", "fit_status", "n_obs", "n_doses", "dose_min", "dose_max", "response_min", "response_max", "message") %in% names(quality)))
  expect_true(all(quality$fit_status == "ok"))
  expect_true(all(quality$n_obs > 1))
  expect_s3_class(failures, "data.frame")
  expect_equal(failures$ID, "999")
  expect_true("message" %in% names(failures))
})

test_that("fit_failures preserves columns when there are no failures", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  failures <- fit_failures(fit)

  expect_s3_class(failures, "data.frame")
  expect_equal(nrow(failures), 0)
  expect_named(failures, c("ID", "model", "message"))
})

test_that("predict_ec50 predicts requested doses for selected models", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  expect_no_warning(all_predictions <- predict_ec50(fit, dose = c(0.001, 0.01)))
  best_predictions <- predict_ec50(fit, dose = c(0.001, 0.01), models = "best")
  named_predictions <- predict_ec50(fit, dose = c(0.001, 0.01), models = "LL.3")

  expect_equal(nrow(all_predictions), 4)
  expect_equal(nrow(best_predictions), 2)
  expect_equal(nrow(named_predictions), 2)
  expect_equal(unique(named_predictions$model), "LL.3")
  expect_true(all(c("ID", "model", "dose", "predicted") %in% names(all_predictions)))
})

test_that("report_ec50 filters all, best, and named models", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3) & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  expect_equal(nrow(report_ec50(fit)), 4)
  expect_equal(nrow(report_ec50(fit, models = "best")), 2)
  expect_equal(unique(report_ec50(fit, models = "LL.3")$model), "LL.3")
  expect_false(inherits(report_ec50(fit), "ec50_estimate"))
})

test_that("check_ec50_data flags common data problems", {
  data(multi_isolate)
  sample_data <- subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A")
  sample_data$growth[1] <- NA
  flat_group <- sample_data[1:2, , drop = FALSE]
  flat_group$isolate <- 999
  flat_group$growth <- 1
  sample_data <- rbind(sample_data, flat_group)

  checks <- check_ec50_data(
    sample_data,
    response = "growth",
    dose = "dose",
    isolate = "isolate",
    strata = "field"
  )

  expect_s3_class(checks, "data.frame")
  expect_true(any(checks$missing_response > 0))
  expect_true(any(checks$nonpositive_dose > 0))
  expect_true(any(checks$too_few_doses))
  expect_true(any(checks$no_response_variation))
})

test_that("check_ec50_data validates numeric response and dose columns", {
  data(multi_isolate)
  sample_data <- subset(multi_isolate, isolate == 1 & fungicida == "Fungicide A")
  bad_dose <- sample_data
  bad_dose$dose <- factor(bad_dose$dose)
  bad_response <- sample_data
  bad_response$growth <- as.character(bad_response$growth)

  expect_error(
    check_ec50_data(
      bad_dose,
      response = "growth",
      dose = "dose",
      isolate = "isolate"
    ),
    "numeric response and dose"
  )
  expect_error(
    check_ec50_data(
      bad_response,
      response = "growth",
      dose = "dose",
      isolate = "isolate"
    ),
    "numeric response and dose"
  )
})

test_that("check_ec50_data preserves columns for empty data", {
  data(multi_isolate)
  empty_data <- multi_isolate[0, ]

  checks <- check_ec50_data(
    empty_data,
    response = "growth",
    dose = "dose",
    isolate = "isolate",
    strata = "field"
  )

  expect_s3_class(checks, "data.frame")
  expect_equal(nrow(checks), 0)
  expect_named(
    checks,
    c(
      "ID", "field", "n_obs", "n_doses", "missing_response",
      "missing_dose", "nonpositive_dose", "duplicated_rows",
      "no_response_variation", "too_few_observations", "too_few_doses"
    )
  )
})

test_that("residual_data and plot_residuals use stored models", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  expect_no_warning(residuals <- residual_data(fit))
  plot <- plot_residuals(fit)
  dose_plot <- plot_residuals(fit, type = "dose", models = "LL.3")

  expect_s3_class(residuals, "data.frame")
  expect_true(all(c("ID", "model", "dose", "observed", "fitted", "residual") %in% names(residuals)))
  expect_s3_class(plot, "ggplot")
  expect_s3_class(dose_plot, "ggplot")
})

test_that("plot_EC50_curves supports model selection filters", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  best_plot <- plot_EC50_curves(fit, models = "best")
  named_plot <- plot_EC50_curves(fit, models = "LL.3")

  expect_s3_class(best_plot, "ggplot")
  expect_equal(length(unique(best_plot$curve_data$model)), 1)
  expect_equal(unique(named_plot$curve_data$model), "LL.3")
})
