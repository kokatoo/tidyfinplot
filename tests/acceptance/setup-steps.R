library(cucumber)
library(tidyfinplot)

spy_data <- readRDS(system.file("extdata", "spy.rds", package = "tidyfinplot"))

given("I have SPY data", function(context) {
  context$spy <- spy_data
})

when("I plot candlestick chart", function(context) {
  context$plot <- plot_candlestick(context$spy)
})

then("I should see a ggplot", function(context) {
  testthat::expect_s3_class(context$plot, "ggplot")
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

# Helper function (put at top of steps.R)
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
