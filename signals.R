# R/signals.R
# Functions for calculating technical indicators and generating trading signals

#' Calculate Exponential Moving Average (EMA)
add_ema <- function(data, n, price_col = "close") {
  
  # Extract price column
  prices <- dplyr::pull(data, {{ price_col }})
  
  # Use TTR's EMA function (battle-tested)
  ema <- TTR::EMA(prices, n = n)
  
  # Add EMA column to data
  col_name <- paste0("ema_", n)
  data <- data |>
    dplyr::mutate({{ col_name }} := ema)
  
  return(data)
}


#' Generate EMA crossover signals
#'
#' @param data A tsibble with EMA columns
#' @param fast_ema Character. Name of fast EMA column (e.g., "ema_10")
#' @param slow_ema Character. Name of slow EMA column (e.g., "ema_20")
#' 
#' @return The input tsibble with added signal columns
#' 
#' @details
#' Signal logic (long-only):
#' - position = 1 when fast EMA > slow EMA (bullish)
#' - position = 0 when fast EMA <= slow EMA (flat)
#' 
#' We also create a 'signal' column that indicates trade events:
#' - signal = 1 when entering long (buy)
#' - signal = -1 when exiting long (sell)
#' - signal = 0 when no trade
#' 
#' @examples
#' spy_data |>
#'   add_ema(10) |>
#'   add_ema(20) |>
#'   generate_crossover_signals("ema_10", "ema_20")
generate_crossover_signals <- function(data, fast_ema, slow_ema) {
  
  data <- data |>
    dplyr::mutate(
      position = dplyr::if_else(
        lag(.data[[fast_ema]]) > lag(.data[[slow_ema]]),
        1,
        0,
        missing = 0
      ),

      signal = position - dplyr::lag(position, default = 0)
    )
  
  return(data)
}


#' Summarize trading signals
#'
#' @param data A tsibble with position and signal columns
#' 
#' @return A tibble with signal summary statistics
#' 
#' @details
#' Provides high-level overview of the strategy's trading activity
summarize_signals <- function(data) {
  
  summary <- tibble::tibble(
    metric = c(
      "Total observations",
      "Periods in market (long)",
      "Periods out of market",
      "Percent time in market",
      "Number of trades (round-trips)",
      "Long entries (buy signals)",
      "Long exits (sell signals)"
    ),
    value = c(
      nrow(data),
      sum(data$position == 1, na.rm = TRUE),
      sum(data$position == 0, na.rm = TRUE),
      paste0(round(100 * mean(data$position == 1, na.rm = TRUE), 1), "%"),
      sum(data$signal == 1, na.rm = TRUE), 
      sum(data$signal == 1, na.rm = TRUE),
      sum(data$signal == -1, na.rm = TRUE)
    )
  )
  
  return(summary)
}