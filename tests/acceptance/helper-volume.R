library(cucumber)

when("I plot volume chart", function(context) {
  context$plot <- plot_volume(context$spy)
})

then("all volume values should be positive", function(context) {
  testthat::expect_true(all(context$spy$volume > 0, na.rm = TRUE))
})

then("I should see up and down colors", function(context) {
  built <- ggplot2::ggplot_build(context$plot)$data
  candles <- candle_palette()
  fills <- unique(built[[1]]$fill)

  testthat::expect_true(candles["up"] %in% fills)
  testthat::expect_true(candles["down"] %in% fills)
})
