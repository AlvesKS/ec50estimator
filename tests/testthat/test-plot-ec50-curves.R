test_that("plot_EC50_curves returns a ggplot for a single model", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate %in% c(1, 3) & field == "Organic" & fungicida == "Fungicide A"
  )

  plot <- plot_EC50_curves(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 2)
  expect_s3_class(plot$facet, "FacetNull")
  expect_true(is.factor(plot$layers[[1]]$data$isolate))
  expect_true(is.factor(plot$layers[[2]]$data$isolate))
  expect_s3_class(attr(plot, "curve_data"), "data.frame")
  expect_s3_class(attr(plot, "observed_data"), "data.frame")
  expect_identical(plot$curve_data, attr(plot, "curve_data"))
  expect_identical(plot$observed_data, attr(plot, "observed_data"))
})

test_that("plot_EC50_curves uses fitted package output without repeated inputs", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate %in% c(1, 3) & field == "Organic" & fungicida == "Fungicide A"
  )

  fit <- estimate_EC50(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = drc::LL.3(),
    quiet = TRUE
  )
  plot <- plot_EC50_curves(fit)
  curves <- curve_data(fit)

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(attr(plot, "curve_data")$model), "LL.3")
  expect_equal(length(attr(plot, "fitted_models")), 2)
  expect_equal(nrow(plot$curve_data), nrow(curves))
  expect_equal(plot$curve_data$dose, curves$dose)
  expect_equal(plot$curve_data$growth, curves$growth)
})

test_that("plot_EC50_curves supports multiple models", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )

  plot <- plot_EC50_curves(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )

  expect_s3_class(plot, "ggplot")
  expect_true("model" %in% names(plot$layers[[2]]$data))
  expect_equal(unique(plot$layers[[2]]$data$model), c("LL.3", "LL.4"))
})

test_that("plot_EC50_curves supports multimodel output directly", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )

  fit <- ec50_multimodel(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = list(drc::LL.3(), drc::LL.4()),
    quiet = TRUE
  )
  plot <- plot_EC50_curves(fit)

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(attr(plot, "curve_data")$model), c("LL.3", "LL.4"))
  expect_true("linetype" %in% names(plot$layers[[2]]$mapping))
})

test_that("plot_EC50_curves validates columns and formulas", {
  data(multi_isolate)

  expect_error(
    plot_EC50_curves(
      growth ~ dose,
      data = multi_isolate,
      isolate_col = "missing",
      fct = drc::LL.3()
    ),
    "columns not found"
  )

  expect_error(
    plot_EC50_curves(
      growth ~ dose + isolate,
      data = multi_isolate,
      isolate_col = "isolate",
      fct = drc::LL.3()
    ),
    "one response and one predictor"
  )
})

test_that("plot_EC50_curves warns for failed group fits", {
  data(multi_isolate)
  good_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )
  bad_data <- good_data[1, , drop = FALSE]
  bad_data$isolate <- 999
  sample_data <- rbind(good_data, bad_data)

  expect_warning(
    plot <- plot_EC50_curves(
      growth ~ dose,
      data = sample_data,
      isolate_col = "isolate",
      fct = drc::LL.3()
    ),
    "could not be produced"
  )
  expect_s3_class(plot, "ggplot")
})

test_that("plot_EC50_curves handles zero doses on a log x-axis", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate == 1 & field == "Organic" & fungicida == "Fungicide A"
  )

  plot <- plot_EC50_curves(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    fct = drc::LL.3(),
    log_x = TRUE,
    quiet = TRUE
  )

  expect_s3_class(plot, "ggplot")
  expect_true(all(plot$layers[[1]]$data$dose > 0))
  expect_true(all(plot$layers[[2]]$data$dose > 0))
})

test_that("plot_EC50_curves uses sensible faceting defaults", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate %in% c(1, 3) & fungicida == "Fungicide A"
  )

  one_stratum <- plot_EC50_curves(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    strata_col = "field",
    fct = drc::LL.3(),
    quiet = TRUE
  )
  expect_s3_class(one_stratum$facet, "FacetWrap")

  two_strata <- plot_EC50_curves(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3)),
    isolate_col = "isolate",
    strata_col = c("field", "fungicida"),
    fct = drc::LL.3(),
    quiet = TRUE
  )
  expect_s3_class(two_strata$facet, "FacetGrid")
})

test_that("plot_EC50_curves uses the first two strata as facet grid dimensions", {
  data(multi_isolate)
  fit <- estimate_EC50(
    growth ~ dose,
    data = subset(multi_isolate, isolate %in% c(1, 3)),
    isolate_col = "isolate",
    strata_col = c("field", "fungicida"),
    fct = drc::LL.3(),
    quiet = TRUE
  )

  plot <- plot_EC50_curves(fit)

  expect_s3_class(plot$facet, "FacetGrid")
  expect_true(all(c("field", "fungicida") %in% names(attr(plot, "curve_data"))))
})

test_that("plot_EC50_curves carries explicit plotting columns into predictions", {
  data(multi_isolate)
  sample_data <- subset(
    multi_isolate,
    isolate %in% c(1, 3) & fungicida == "Fungicide A"
  )

  plot <- plot_EC50_curves(
    growth ~ dose,
    data = sample_data,
    isolate_col = "isolate",
    color_col = "field",
    facet_col = "field",
    fct = drc::LL.3(),
    quiet = TRUE
  )

  expect_s3_class(plot, "ggplot")
  expect_true("field" %in% names(plot$layers[[2]]$data))
  expect_s3_class(plot$facet, "FacetWrap")
})
