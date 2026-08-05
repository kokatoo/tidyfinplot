devtools::load_all()

spy_orig <- readRDS("inst/extdata/spy.rds")

spy <- spy_orig |>
  filter(date >= "2026-01-01")

plot_candlestick(spy)
