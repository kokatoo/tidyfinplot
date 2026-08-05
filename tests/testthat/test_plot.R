test_that("plot_candlestick returns a ggplot", {
  p <- plot_candlestick(spy)

  expect_s3_class(p, "ggplot")
})
