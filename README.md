# tidyfinplot

[![R CI](https://github.com/kokatoo/tidyfinplot/actions/workflows/check.yml/badge.svg)](https://github.com/kokatoo/tidyfinplot/actions/workflows/check.yml)
[![codecov](https://codecov.io/github/kokatoo/tidyfinplot/graph/badge.svg?token=DT2T222N6Y)](https://codecov.io/github/kokatoo/tidyfinplot)

Tidyverse-style financial charting helpers built on tidyquant and ggplot2.

## Installation

```r
devtools::install_github("kokatoo/tidyfinplot")
```

## Quick start

```r
library(tidyfinplot)

# Sample SPY data bundled with the package
spy <- readRDS(system.file("extdata", "spy.rds", package = "tidyfinplot"))

# Candlestick chart with moving average overlays
plot_candlestick(spy)

# Volume bars colored by up/down days
plot_volume(spy)
```

## Features

- Candlestick charts with automatic OHLC mapping
- Moving average overlays (SMA 5, 20, 50, 200)
- Volume bars colored by price direction
- Built-in color palettes, all customizable

## License

This project is licensed under the MIT License. See
[LICENSE.md](LICENSE.md) for details.
