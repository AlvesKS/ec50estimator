test_that("ec50_estimates returns a plain data frame", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3) & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    strata_col = "field",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  estimates <- ec50_estimates(fit)

  expect_s3_class(estimates, "data.frame")
  expect_false(inherits(estimates, "ec50_estimate"))
  expect_named(estimates, names(as.data.frame(fit)))
  expect_equal(row.names(estimates), as.character(seq_len(nrow(estimates))))
})

test_that("ec50_metadata returns stable modeling metadata", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    strata_col = "field",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  metadata <- ec50_metadata(fit)

  expect_equal(metadata$formula, growth ~ dose)
  expect_equal(metadata$isolate_col, "isolate")
  expect_equal(metadata$strata_col, "field")
  expect_equal(metadata$model_labels, c("LL.3", "LL.4"))
  expect_equal(metadata$n_models, 2)
  expect_true(all(c("isolate", "field", "fungicida", "dose", "growth") %in% metadata$data_columns))
})

test_that("fitted_models returns named drc model objects", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3) & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  models <- fitted_models(fit)

  expect_type(models, "list")
  expect_equal(length(models), 2)
  expect_true(all(nzchar(names(models))))
  expect_true(all(vapply(models, inherits, logical(1), "drc")))
})

test_that("curve_data returns fitted coordinates from stored models", {
  data(multi_isolate)
  fit <- ec50_multimodel(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  curves <- curve_data(fit, n_points = 25)

  expect_s3_class(curves, "data.frame")
  expect_equal(nrow(curves), 50)
  expect_true(all(c("isolate", "model", "dose", "growth", ".curve_group") %in% names(curves)))
  expect_equal(unique(curves$model), c("LL.3", "LL.4"))
  expect_true(all(curves$dose > 0))
})

test_that("curve_data handles zero-dose rows on log x-axis", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate == 1 & field == "Organic" & fungicida == "Fungicide A"),
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  curves <- curve_data(fit, log_x = TRUE)

  expect_s3_class(curves, "data.frame")
  expect_true(all(curves$dose > 0))
})

test_that("extractors validate EC50 fit objects", {
  expect_error(ec50_estimates(data.frame()), "estimate_EC50")
  expect_error(ec50_metadata(data.frame()), "estimate_EC50")
  expect_error(fitted_models(data.frame()), "estimate_EC50")
  expect_error(curve_data(data.frame()), "estimate_EC50")
})
