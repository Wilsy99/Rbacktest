add_ema_crossover_strategy <- function(data, price_col = "close", ns) {
  fast_ema_n <- min(ns)
  slow_ema_n <- max(ns)

  fast_ema <- sym(paste0("ema_", fast_ema_n))
  slow_ema <- sym(paste0("ema_", slow_ema_n))

  data |>
    add_emas(price_col = price_col, ns = ns) |>
    mutate(
      signal = case_when(
        (!!fast_ema > !!slow_ema) & (lag(!!fast_ema) < lag(!!slow_ema)) ~ 1,
        (!!fast_ema < !!slow_ema) & (lag(!!fast_ema) > lag(!!slow_ema)) ~ -1,
        .default = 0
      ),
      position = as.numeric(lag(!!fast_ema) > lag(!!slow_ema)) *
        pmin(cumsum(pmax(signal, 0)), 1)
    )
}

add_emas <- function(data, price_col = "close", ns) {
  prices <- pull(data, {{ price_col }})

  emas <-
    map(ns, \(n) EMA(prices, n)) |>
    set_names(paste0("ema_", ns)) |>
    as_tibble()

  bind_cols(data, emas)
}

# identify_trades <- function(data) {
#   data |>
#     mutate(
#       signal = if_else(
#         week_end = last(week_end) & position = 1,
#         signal = -1,
#         signal
#       )
#     ) |>
#     filter(
#       signal != 0
#     ) |>
#     filter(
#       !(row_number() == 1 & signal == -1),
#     ) |>
#     mutate(trade_id = cumsum(signal == 1)) |>
#     summarise(
#       buy_date = first(week_end),
#       sell_date = last(week_end),
#       buy_price = first(close),
#       sell_price = last(close),
#       return = sell_price / buy_price - 1,
#       .by = trade_id
#     )
# }
