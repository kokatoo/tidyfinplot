#' SMA color palette
#' @return Named character vector of SMA colors
#' @export
sma_palette <- function() {
  c(
    SMA_5 = "goldenrod",
    SMA_20 = "forestgreen",
    SMA_50 = "navy",
    SMA_200 = "darkred"
  )
}

#' Candle color palette
#' @return Named character vector of up/down candle colors
#' @export
candle_palette <- function() {
  c(
    up = "darkgreen",
    down = "darkred"
  )
}
