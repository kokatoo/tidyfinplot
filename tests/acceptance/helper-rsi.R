library(cucumber)

when("I plot RSI chart", function(context) {
  context$plot <- plot_rsi(context$spy)
})

then("RSI should be bounded by 0 and 100", function(context) {
  built <- ggplot2::ggplot_build(context$plot)
  panel_params <- built$layout$panel_params[[1]]

  y_axis <- panel_params$y
  y_limits <- y_axis$get_limits()

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

when("I plot RSI chart with multiple periods", function(context) {
  context$plot <- plot_rsi(
    context$spy,
    rsi_col = c("RSI_9", "RSI_14", "RSI_21")
  )
})

then("I should see 3 RSI lines", function(context) {
  n_lines <- sum(vapply(
    context$plot$layers,
    function(x) "GeomLine" %in% class(x$geom),
    logical(1)
  ))

  testthat::expect_equal(n_lines, 3)
})
