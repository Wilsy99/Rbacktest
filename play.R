library(dplyr)
library(lubridate)
library(TTR)
library(tidyquant)
library(purrr)

spy_data <- get_weekly_data("SPY")

ema_cross_strategy <-
  spy_data |>
  add_ema_crossover_strategy(ns = c(10, 20)) |>
  calculate_strategy_returns()

paste0(
  round(
    (prod(1 + ema_cross_strategy$strategy_gross_returns, na.rm = TRUE) - 1) *
      100,
    2
  ),
  "%"
)

spy_data |>
  add_ema_crossover_strategy(ns = c(10, 20)) |>
  identify_trades() |>
  View()
