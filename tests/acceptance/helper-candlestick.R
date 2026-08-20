library(cucumber)

when("I plot candlestick chart", function(context) {
  context$plot <- plot_candlestick(context$spy)
})

when("I plot candlestick chart with monthly breaks", function(context) {
  context$plot <- plot_candlestick(
    context$spy,
    date_breaks = "1 month",
    date_labels = "%b %y"
  )
})

then("I should see candlesticks", function(context) {
  geoms <- vapply(
    context$plot$layers,
    function(x) class(x$geom)[1],
    character(1)
  )
  testthat::expect_true("GeomRectCS" %in% geoms)
})

then("I should see SMA_5 line", function(context) {
  testthat::expect_true(has_sma(context$plot, "SMA_5"))
})

then("I should see SMA_20 line", function(context) {
  testthat::expect_true(has_sma(context$plot, "SMA_20"))
})

then("I should see SMA_50 line", function(context) {
  testthat::expect_true(has_sma(context$plot, "SMA_50"))
})

then("I should see SMA_200 line", function(context) {
  testthat::expect_true(has_sma(context$plot, "SMA_200"))
})

then("I should see SMA lines in palette colors", function(context) {
  built <- ggplot2::ggplot_build(context$plot)$data
  palette <- unname(sma_palette())
  colors_seen <- character(0)
  for (i in seq_along(context$plot$layers)) {
    if ("GeomLine" %in% class(context$plot$layers[[i]]$geom)) {
      colors_seen <- c(colors_seen, unique(built[[i]]$colour))
    }
  }
  testthat::expect_setequal(colors_seen, palette)
})

then("I should see up and down candle colors", function(context) {
  built <- ggplot2::ggplot_build(context$plot)$data
  candles <- candle_palette()
  for (i in seq_along(context$plot$layers)) {
    if ("GeomRectCS" %in% class(context$plot$layers[[i]]$geom)) {
      fills <- unique(built[[i]]$fill)
      testthat::expect_true(candles["up"] %in% fills)
      testthat::expect_true(candles["down"] %in% fills)
    }
  }
})

# Helper to check whether a plot has an SMA line layer
has_sma <- function(p, sma_name) {
  for (layer in p$layers) {
    if ("GeomLine" %in% class(layer$geom)) {
      y_var <- rlang::as_label(layer$mapping$y)
      if (y_var == sma_name) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
