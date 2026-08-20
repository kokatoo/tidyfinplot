devtools::load_all()

library(patchwork)

spy_orig <- readRDS("inst/extdata/spy.rds")

spy <- spy_orig |>
  filter(date >= "2026-01-01")

p1 <- plot_candlestick(spy, date_breaks = "1 month", date_labels = "%b %y")

p2 <- plot_volume(spy, date_breaks = "1 month", date_labels = "%b %y")

p1 / p2 + patchwork::plot_layout(heights = c(0.7, 0.3))
