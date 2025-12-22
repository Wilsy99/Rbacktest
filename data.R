get_daily_data <- function(
  ticker,
  from = "1900-01-01",
  to = Sys.Date(),
  calculate_returns = TRUE
) {
  data <-
    tq_get(
      x = ticker,
      get = "stock.prices",
      from = from,
      to = to
    ) |>
    roworder(date)

  adj_factor <- data$adjusted / data$close

  adjusted_data <-
    data |>
    ftransform(
      open = open * adj_factor,
      high = high * adj_factor,
      low = low * adj_factor,
      close = adjusted
    ) |>
    fselect(-adjusted)

  if (!calculate_returns) {
    return(adjusted_data)
  }

  adjusted_data |>
    fmutate(return = fgrowth(close, 1, scale = 1))
}

get_weekly_data <- function(
  ticker,
  week_start = 1,
  from = "1900-01-01",
  to = Sys.Date(),
  calculate_returns = TRUE
) {
  data <-
    get_daily_data(
      ticker = ticker,
      from = from,
      to = to,
      calculate_returns = FALSE
    ) |>
    fmutate(
      week_start = floor_date(date, unit = "weeks", week_start = week_start)
    ) |>
    fgroup_by(week_start) |>
    fsummarise(
      week_end = flast(date),
      open = ffirst(open),
      high = fmax(high),
      low = fmin(low),
      close = flast(close),
      volume = fsum(volume)
    )

  if (!calculate_returns) {
    return(data)
  }

  data |>
    fmutate(return = fgrowth(close, 1, scale = 1))
}
