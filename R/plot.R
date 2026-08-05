#' #' Plot candlestick with SMAs
#'
#' @param spy SPY data frame with OHLC and SMA columns
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr tidyquant scales
plot_candlestick <- function(data) {
  colors <- sma_palette()
  candles <- candle_palette()

  ggplot(data, aes(x = date, y = close)) +
    tidyquant::geom_candlestick(
      aes(open = open, high = high, low = low, close = close),
      colour_up = candles["up"],
      colour_down = candles["down"],
      fill_up = candles["up"],
      fill_down = candles["down"]
    ) +
    geom_line(aes(y = SMA_5), color = colors["SMA_5"]) +
    geom_line(aes(y = SMA_20), color = colors["SMA_20"]) +
    geom_line(aes(y = SMA_50), color = colors["SMA_50"]) +
    geom_line(aes(y = SMA_200), color = colors["SMA_200"]) +
    scale_x_date(
      date_breaks = "3 month",
      date_labels = "%b %y"
    ) +
    scale_y_continuous(
      breaks = scales::breaks_width(10),
      sec.axis = sec_axis(~ ., breaks = scales::breaks_width(10))
    ) +
    theme_minimal()
}
