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
