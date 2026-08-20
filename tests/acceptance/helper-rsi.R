library(cucumber)

when("I plot RSI chart", function(context) {
  context$plot <- plot_rsi(context$spy)
})

then("RSI should be bounded by 0 and 100", function(context) {
  y_limits <- ggplot2::ggplot_build(context$plot)$layout$panel_params[[1]]$y$get_limits()
  testthat::expect_equal(y_limits, c(0, 100))
})

then("I should see horizontal lines at 30 and 70", function(context) {
  built <- ggplot2::ggplot_build(context$plot)$data
  yintercepts <- unlist(lapply(built, function(d) {
    if ("yintercept" %in% names(d)) unique(d$yintercept)
  }))
  testthat::expect_true(30 %in% yintercepts)
  testthat::expect_true(70 %in% yintercepts)
})
