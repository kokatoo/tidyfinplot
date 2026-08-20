test_that("plot_rsi returns a ggplot", {
  p <- plot_rsi(spy)
  expect_s3_class(p, "ggplot")
})

test_that("plot_rsi y axis is bounded by 0 and 100", {
  p <- plot_rsi(spy)
  expect_s3_class(p, "ggplot")
  y_limits <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_limits()
  expect_equal(y_limits, c(0, 100))
})

test_that("plot_rsi draws horizontal lines at 30 and 70", {
  p <- plot_rsi(spy)
  expect_s3_class(p, "ggplot")

  yintercepts <- unlist(lapply(p$layers, function(x) {
    if ("GeomHline" %in% class(x$geom)) x$aes_params$yintercept
  }))

  expect_true(30 %in% yintercepts)
  expect_true(70 %in% yintercepts)
})

test_that("plot_rsi has no axis titles", {
  p <- plot_rsi(spy)
  expect_s3_class(p, "ggplot")

  expect_true(is.null(p$labels$x) || p$labels$x == "")
  expect_true(is.null(p$labels$y) || p$labels$y == "")
})

test_that("plot_rsi works with a custom RSI column", {
  p <- plot_rsi(spy, rsi_col = "RSI_21")
  expect_s3_class(p, "ggplot")
})
