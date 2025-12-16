# current use case

weekly_data <- get_weekly_data(
  symbol = "SPY", 
  from = "1900-01-01"
)

weekly_signals <-
  weekly_data |> 
  add_ema(n = 10, price_col = "adjusted") |> 
  add_ema(n = 20, price_col = "adjusted") |> 
  generate_crossover_signals(fast_ema = "ema_10", slow_ema = "ema_20")

 result <- 
  weekly_signals |>
  calculate_returns() |> 
  calculate_benchmark_returns() |> 
  calculate_strategy_returns()

metrics <- compute_performance_metrics(
  result$net_return,
  periods_per_year = 52,
  n_trials = 1,
  rf_annual = 0.04
)

print_validation_summary(metrics, "SPY EMA 10/20")

summarize_returns(result)
