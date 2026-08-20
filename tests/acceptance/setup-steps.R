library(cucumber)
library(tidyfinplot)

spy_data <- readRDS(system.file("extdata", "spy.rds", package = "tidyfinplot"))

given("I have SPY data", function(context) {
  context$spy <- spy_data
})

then("I should see a ggplot", function(context) {
  testthat::expect_s3_class(context$plot, "ggplot")
})

then("I should see no axis titles", function(context) {
  testthat::expect_true(
    is.null(context$plot$labels$x) || context$plot$labels$x == ""
  )
  testthat::expect_true(
    is.null(context$plot$labels$y) || context$plot$labels$y == ""
  )
})
