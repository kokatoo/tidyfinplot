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

  built <- ggplot2::ggplot_build(p)$data
  yintercepts <- unlist(lapply(built, function(d) {
    if ("yintercept" %in% names(d)) unique(d$yintercept)
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

test_that("plot_rsi shades between the line and the thresholds", {
  p <- plot_rsi(spy)
  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)$data
  fills <- vapply(built, function(d) paste(unique(d$fill), collapse = ","), character(1))
  candles <- candle_palette()

  red <- built[[which(fills == unname(candles["down"]))]]
  green <- built[[which(fills == unname(candles["up"]))]]

  # Overbought zone: ribbon pinned at 70, reaching up to the line
  expect_true(all(red$ymin == 70, na.rm = TRUE))
  expect_true(max(red$ymax, na.rm = TRUE) > 70)

  # Oversold zone: ribbon pinned at 30, reaching down to the line
  expect_true(all(green$ymax == 30, na.rm = TRUE))
  expect_true(min(green$ymin, na.rm = TRUE) < 30)
})
