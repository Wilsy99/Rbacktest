transform_to_weekly <- function(data, week_start = 1) {
  data |> 
    dplyr::arrange(date) |> 
    dplyr::mutate(
      adjustment_factor = adjusted / close,
      week_start = lubridate::floor_date(date, unit = "weeks", week_start = 1),
    ) |> 
    dplyr::summarise(
      week_end = dplyr::last(date),
      open = dplyr::first(open * adjustment_factor),
      high = max(high * adjustment_factor),
      low = min(low * adjustment_factor),
      close = dplyr::last(adjusted),
      volume = sum(volume),
      .by = week_start
    )
}
  
add_ema <- function(data, price_col, n) {
  ema_col_name <- paste0("ema_", n)

  ema <- 
    data |> 
    dplyr::pull({{price_col}}) |> 
    TTR::EMA(n)

  data |> 
    dplyr::mutate(
      {{ema_col_name}} := ema
    )
}

sum_ignore_na <- purrr::partial(.f = sum, na.rm = TRUE)

daily_data <- tidyquant::tq_get(
  x = "SPY",
  get = "stock.prices",
  from = "2010-01-01",
  to = Sys.Date()
)

trades <- daily_data |>
  transform_to_weekly() |>
  add_ema(price_col = "close", n = 10) |>
  add_ema(price_col = "close", n = 20) |>
  dplyr::mutate(
    signal = dplyr::case_when(
      (ema_10 > ema_20) & (dplyr::lag(ema_10) < dplyr::lag(ema_20)) ~ 1,
      (ema_10 < ema_20) & (dplyr::lag(ema_10) > dplyr::lag(ema_20)) ~ -1,
      .default = 0
    ),
    # FIX: Trade at next week's open (shift forward by 1 period)
    next_open = dplyr::lead(open)
  ) |>
  # Remove last row if it has a signal (can't trade next week if data ends)
  dplyr::filter(signal != 0, !is.na(next_open)) |>
  dplyr::slice(-c(
    if (dplyr::first(signal) == -1) 1 else integer(0)
  )) |>
  dplyr::mutate(trade_id = cumsum(signal == 1)) |>
  dplyr::summarise(
    buy_date = dplyr::first(week_end),
    sell_date = dplyr::last(week_end),
    buy_price = dplyr::first(next_open),   # Use next week's open
    sell_price = dplyr::last(next_open),   # Use next week's open
    return = sell_price / buy_price - 1,
    .by = trade_id
  )

prod(1 + trades$return) - 1
