devtools::load_all()

library(patchwork)

spy_orig <- readRDS("inst/extdata/spy.rds")

spy <- spy_orig |>
  filter(date >= "2026-01-01")

last_date <- max(spy$date)
forecast_dates <- seq(last_date + 1, last_date + 21, by = "day")

forecast_tbl <- tibble(
  date = forecast_dates,
  )

spy <- bind_rows(spy, forecast_tbl)


p1 <- plot_candlestick(spy, date_breaks = "1 month", date_labels = "%b %y")

p2 <- plot_volume(spy, date_breaks = "1 month", date_labels = "%b %y")

p1 / p2 +
  patchwork::plot_layout(heights = c(0.7, 0.3))

p3 <- plot_rsi(
  spy,
  rsi_col = c("RSI_9", "RSI_14", "RSI_21"),
  date_breaks = "1 month",
  date_labels = "%b %y"
)
p1 / p3 +
  ggtitle("RSI 9, 14, 21") +
  patchwork::plot_layout(heights = c(0.7, 0.3))
