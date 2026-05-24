test_that("estimate_EC50 returns estimates for stratified isolate data", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate %in% c(1, 3) & field == "Organic" & fungicida == "Fungicide A"
  )

  result <- estimate_EC50(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    strata_col = c("field", "fungicida"),
    fct = drc::LL.3(),
    interval = "none"
  )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "ec50_estimate")
  expect_named(result, c("ID", "field", "fungicida", "Estimate", "Std..Error"))
  expect_equal(result$ID, c("1", "3"))
  expect_true(all(result$Estimate > 0))
  expect_equal(attr(result, "ec50_isolate_col"), "isolate")
  expect_equal(attr(result, "ec50_strata_col"), c("field", "fungicida"))
  expect_equal(length(attr(result, "ec50_models")), 2)
})

test_that("estimate_EC50 validates inputs before fitting", {
  data(multi_isolate)

  expect_error(
    estimate_EC50(
      growth ~ dose,
      data = multi_isolate,
      isolate_col = "missing",
      fct = drc::LL.3()
    ),
    "columns not found"
  )

  expect_error(
    estimate_EC50(
      growth ~ dose,
      data = multi_isolate,
      isolate_col = "isolate",
      strata_col = "missing",
      fct = drc::LL.3()
    ),
    "columns not found"
  )

  non_numeric <- multi_isolate
  non_numeric$dose <- factor(non_numeric$dose)
  expect_error(
    estimate_EC50(
      growth ~ dose,
      data = non_numeric,
      isolate_col = "isolate",
      fct = drc::LL.3()
    ),
    "numeric response and dose"
  )

  expect_error(
    estimate_EC50(
      growth ~ dose + isolate,
      data = multi_isolate,
      isolate_col = "isolate",
      fct = drc::LL.3()
    ),
    "one response and one predictor"
  )
})

test_that("estimate_EC50 preserves structure when all groups fail", {
  data(multi_isolate)
  bad_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )[1, , drop = FALSE]

  expect_silent(
    fit <- estimate_EC50(
      growth ~ dose,
      data = bad_data,
      isolate_col = "isolate",
      strata_col = "field",
      fct = drc::LL.3(),
      quiet = TRUE
    )
  )

  estimates <- ec50_estimates(fit)

  expect_s3_class(fit, "ec50_estimate")
  expect_named(estimates, c("ID", "field", "Estimate", "Std..Error"))
  expect_equal(nrow(estimates), 0)
  expect_equal(nrow(fit_failures(fit)), 1)
})

test_that("estimate_EC50 supports absolute ED levels", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )

  result <- estimate_EC50(
    growth ~ dose,
    data = sample_data,
    EC_lvl = 10,
    isolate_col = "isolate",
    fct = drc::LL.3(),
    type = "absolute",
    quiet = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("ID", "Estimate", "Std..Error"))
})

test_that("ec50_multimodel appends model-selection statistics", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )

  result <- ec50_multimodel(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "ec50_multimodel")
  expect_equal(nrow(result), 2)
  expect_true(all(c("ID", "Estimate", "Std..Error", "model", "IC") %in% names(result)))
  expect_equal(result$model, c("LL.3", "LL.4"))
  expect_equal(length(attr(result, "ec50_models")), 2)
})
