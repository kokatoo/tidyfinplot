#' Plot candlestick with SMAs
#'
#' @param data SPY data frame with OHLC and SMA columns
#' @param colors SMA colors, see [sma_palette()]
#' @param candles Candle up/down colors, see [candle_palette()]
#' @param date_breaks Date break spacing
#' @param date_labels Date label format
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr tidyquant scales
plot_candlestick <- function(data, colors = sma_palette(), candles = candle_palette(),
                             date_breaks = "3 month", date_labels = "%b %y") {

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
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      breaks = scales::breaks_width(10),
      sec.axis = sec_axis(~ ., breaks = scales::breaks_width(10))
    ) +
    labs(x = "", y = "") +
    theme_tq()
}

#' Plot volume below candlestick chart
#'
#' @param data SPY data frame with date, open, close and volume columns
#' @param colors Candle up/down colors, see [candle_palette()]
#' @param date_breaks Date break spacing
#' @param date_labels Date label format
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr scales
plot_volume <- function(data, colors = candle_palette(),
                        date_breaks = "3 month", date_labels = "%b %y") {

  # Determine if each day is up or down based on close vs open
  data <- data |>
    mutate(
      is_up = close >= open,
      vol_color = ifelse(is_up, colors["up"], colors["down"])
    )

  ggplot(data, aes(x = date, y = volume, fill = vol_color)) +
    geom_col(alpha = 0.7) +
    scale_fill_identity() +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale = 1e-6, suffix = "M"),
      sec.axis = sec_axis(
        ~ .,
        labels = scales::label_number(scale = 1e-6, suffix = "M")
      )
    ) +
    labs(x = "", y = "") +
    theme_tq() +
    theme(axis.title.x = element_blank())
}

#' Plot RSI with overbought and oversold thresholds
#'
#' @param data SPY data frame with a date and RSI column
#' @param rsi_col Name of the RSI column to plot
#' @param oversold Lower threshold line
#' @param overbought Upper threshold line
#' @param date_breaks Date break spacing
#' @param date_labels Date label format
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr scales
plot_rsi <- function(data, rsi_col = "RSI_14",
                     oversold = 30, overbought = 70,
                     date_breaks = "3 month", date_labels = "%b %y") {
}
