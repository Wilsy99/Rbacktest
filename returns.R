calculate_strategy_returns <- function(data) {
  data |>
    mutate(strategy_gross_returns = position * return)
}
