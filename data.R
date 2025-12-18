get_daily_data <- function(
  ticker,
  from = "1900-01-01",
  to = Sys.Date(),
  add_returns = TRUE
) {
  data <-
    tq_get(
      x = ticker,
      get = "stock.prices",
      from = from,
      to = to
    ) |>
    arrange(date) |>
    mutate(
      adjustment_factor = adjusted / close,
      open = open * adjustment_factor,
      high = high * adjustment_factor,
      low = low * adjustment_factor,
      close = close * adjustment_factor,
    ) |>
    select(-c(adjusted, adjustment_factor))

  if (!add_returns) {
    return(data)
  }

  data |>
    calculate_returns()
}

get_weekly_data <- function(
  ticker,
  week_start = 1,
  from = "1900-01-01",
  to = Sys.Date(),
  add_returns = TRUE
) {
  data <-
    get_daily_data(
      ticker = ticker,
      from = from,
      to = to,
      add_returns = FALSE
    ) |>
    mutate(
      week_start = floor_date(date, unit = "weeks", week_start = week_start)
    ) |>
    summarise(
      week_end = last(date),
      open = first(open),
      high = max(high),
      low = min(low),
      close = last(close),
      volume = sum(volume),
      .by = week_start
    )

  if (!add_returns) {
    return(data)
  }

  data |>
    calculate_returns()
}

calculate_returns <- function(data, close_col = "close") {
  close_col <- ensym(close_col)
  data |>
    mutate(
      return = !!close_col / lag(!!close_col) - 1
    )
}
