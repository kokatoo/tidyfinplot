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
plot_candlestick <- function(data, colors = sma_palette(),
                             candles = candle_palette(),
                             date_breaks = "3 month",
                             date_labels = "%b %y") {

  scale_breaks <- scales::breaks_width(10)

  ggplot(data, aes(x = date, y = close)) +
    tidyquant::geom_candlestick(
      aes(
        open = open,
        high = high,
        low = low,
        close = close
      ),
      colour_up = candles["up"],
      colour_down = candles["down"],
      fill_up = candles["up"],
      fill_down = candles["down"]
    ) +
    geom_line(aes(y = SMA_5), color = colors["SMA_5"], na.rm = TRUE) +
    geom_line(aes(y = SMA_20), color = colors["SMA_20"], na.rm = TRUE) +
    geom_line(aes(y = SMA_50), color = colors["SMA_50"], na.rm = TRUE) +
    geom_line(aes(y = SMA_200), color = colors["SMA_200"], na.rm = TRUE) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      breaks = scale_breaks,
      sec.axis = sec_axis(~ ., breaks = scale_breaks)
    ) +
    labs(x = "", y = "") +
    theme_tq()
}

#' Plot volume below candlestick chart
#'
#' @param data SPY data frame with OHLC and volume columns
#' @param colors Candle up/down colors, see [candle_palette()]
#' @param date_breaks Date break spacing
#' @param date_labels Date label format
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr scales
plot_volume <- function(data, colors = candle_palette(),
                        date_breaks = "3 month",
                        date_labels = "%b %y") {

  # Determine if each day is up or down based on close vs open
  data <- data |>
    mutate(
      is_up = close >= open,
      vol_color = ifelse(is_up, colors["up"], colors["down"])
    )

  scale_label <- scales::label_number(scale = 1e-6, suffix = "M")

  ggplot(data, aes(x = date, y = volume, fill = vol_color)) +
    geom_col(alpha = 0.7) +
    scale_fill_identity() +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      labels = scale_label,
      sec.axis = sec_axis(
        ~ .,
        labels = scale_label
      )
    ) +
    labs(x = "", y = "") +
    theme_tq() +
    theme(axis.title.x = element_blank())
}

#' Plot RSI with overbought and oversold thresholds
#'
#' @param data SPY data frame with a date and RSI column
#' @param rsi_col Name(s) of the RSI column(s) to plot
#' @param oversold Lower threshold line
#' @param overbought Upper threshold line
#' @param colors Candle up/down colors, see [candle_palette()]
#' @param date_breaks Date break spacing
#' @param date_labels Date label format
#' @return ggplot object
#' @export
#' @import ggplot2 dplyr scales
plot_rsi <- function(data, rsi_col = "RSI_14",
                     oversold = 30,
                     overbought = 70,
                     colors = candle_palette(),
                     date_breaks = "3 month",
                     date_labels = "%b %y") {

  missing <- setdiff(rsi_col, names(data))

  if (length(missing) > 0) {
    stop(
      "RSI columns not found in data: ",
      paste(missing, collapse = ", ")
    )
  }

  ## # Drop warm-up rows
  ## data <- data[complete.cases(data[rsi_col]), , drop = FALSE]

  ## # Row-wise envelope
  data$rsi_max <- apply(data[rsi_col], 1, max)
  data$rsi_min <- apply(data[rsi_col], 1, min)

  n <- length(rsi_col)

  line_colors <- if (n == 1) {
    "navy"
  } else {
    # Interpolate over the darker half of the Blues palette
    grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(9, "Blues")[c(5, 9)]
    )(n)
  }

  p <- ggplot(data, aes(x = date)) +
    geom_ribbon(
      aes(
        ymin = overbought,
        ymax = pmax(rsi_max, overbought, na.rm = TRUE)
      ),
      fill = colors["down"], alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        ymin = pmin(rsi_min, oversold, na.rm = TRUE),
        ymax = oversold
      ),
      fill = colors["up"], alpha = 0.1
    )
  for (i in seq_along(rsi_col)) {
    p <- p + geom_line(
      aes(y = !!rlang::sym(rsi_col[i])),
      color = line_colors[i],
      na.rm = TRUE
    )
  }

  scale_breaks <- seq(0, 100, by = 20)
  p +
    geom_hline(
      yintercept = c(oversold, overbought),
      linetype = "dashed",
      color = c(colors["up"], colors["down"])
    ) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = scale_breaks,
      sec.axis = sec_axis(~ ., breaks = scale_breaks)
    ) +
    labs(x = "", y = "") +
    theme_tq()
}
