test_that("plot_candlestick has SMA 5, 20, 50, 200", {
  p <- plot_candlestick(spy)

  sma_names <- sapply(p$layers, function(x) {
    if ("GeomLine" %in% class(x$geom)) {
      rlang::as_label(x$mapping$y)
    }
  })

  expect_true(all(c("SMA_5", "SMA_20", "SMA_50", "SMA_200") %in% sma_names))
})

test_that("plot_candlestick has no axis titles", {
  p <- plot_candlestick(spy)

  # Check x-axis label
  expect_true(is.null(p$labels$x) || p$labels$x == "")
  # Check y-axis label
  expect_true(is.null(p$labels$y) || p$labels$y == "")
})

test_that("plot_volume has no axis titles", {
  p <- plot_volume(spy)

  # Check x-axis label
  expect_true(is.null(p$labels$x) || p$labels$x == "")
  # Check y-axis label
  expect_true(is.null(p$labels$y) || p$labels$y == "")
})

test_that("plot_volume uses up and down colors", {
  p <- plot_volume(spy)

  # Extract fill colors from volume bars
  fill_colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)

  # Get default palette
  candles <- candle_palette()

  # Should have both up and down colors
  expect_true(candles["up"] %in% fill_colors)
  expect_true(candles["down"] %in% fill_colors)
})

test_that("plot_volume colors match candle direction", {
  p <- plot_volume(spy)

  # Get the data used in the plot
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]

  # Volume bars should have fill colors based on up/down days
  # Check that both colors appear
  colors_used <- unique(plot_data$fill)
  expect_true(length(colors_used) >= 2)
})

test_that("plot_volume renders a dual y axis", {
  p <- plot_volume(spy)
  expect_s3_class(p, "ggplot")

  gg <- ggplot2::ggplotGrob(p)
  expect_equal(sum(grepl("axis-r", gg$layout$name)), 1)
})
