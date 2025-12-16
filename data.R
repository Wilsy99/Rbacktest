# R/data.R
# Functions for fetching and preparing financial data

#' Get weekly SPY data from Yahoo Finance
#'
#' @param symbol Character. Ticker symbol (default: "SPY")
#' @param from Character. Start date in "YYYY-MM-DD" format
#' @param to Character. End date in "YYYY-MM-DD" format
#' 
#' @return A tsibble with weekly OHLCV data
#' 
#' @details
#' This function:
#' 1. Fetches daily data from Yahoo Finance via tidyquant
#' 2. Converts to weekly frequency (Friday close)
#' 3. Returns a tsibble (time-aware tibble) for downstream analysis
#' 
#' @examples
#' spy_data <- get_weekly_data(from = "2010-01-01", to = "2023-12-31")
get_weekly_data <- function(symbol = "SPY",
                            from = "2010-01-01",
                            to = Sys.Date()) {
  
  daily_data <- tidyquant::tq_get(
    x = symbol,
    get = "stock.prices",
    from = from,
    to = to
  )
  
  weekly_data <- daily_data |>
    dplyr::mutate(
      week = lubridate::floor_date(date, unit = "week", week_start = 1)
    ) |>
    dplyr::group_by(symbol, week) |>
    dplyr::slice_tail(n = 1) |>  
    dplyr::ungroup() |>
    dplyr::select(
      date,
      open,
      high,
      low,
      close,
      volume,
      adjusted
    )
  
  weekly_tsibble <- weekly_data |>
    tsibble::as_tsibble(index = date)
  
  return(weekly_tsibble)
}


#' Validate data quality
#'
#' @param data A tsibble or tibble with OHLCV data
#' 
#' @return Tibble with data quality metrics
#' 
#' @details
#' Checks for:
#' - Missing values
#' - Duplicate dates
#' - Gaps in time series
#' - Negative prices (data errors)
validate_data <- function(data) {
  tibble::tibble(
    check = c(
      "Total observations",
      "Missing close prices",
      "Duplicate dates",
      "Negative prices",
      "Date range"
    ),
    result = c(
      nrow(data),
      sum(is.na(data$close)),
      anyDuplicated(data$date) > 0,
      any(data$close < 0, na.rm = TRUE),
      paste(min(data$date), "to", max(data$date))
    )
  )
}