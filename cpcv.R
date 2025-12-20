generate_splits <- function(data, n_groups, n_test_groups) {
  data_with_groups <-
    data |>
    mutate(group_id = ntile(n = n_groups))

  tibble(
    test_groups = combn(n_groups, n_test_groups, simplify = FALSE),
    training_groups = map(test_groups, \(test_group) {
      setdiff(1:n_groups, test_group)
    }),
    split_id = seq_along(test_groups),
    training_set = map(training_groups, \(tg) {
      filter(data_with_groups, group_id %in% tg)
    }),
    test_set = map(test_groups, \(tg) {
      filter(data_with_groups, group_id %in% tg)
    })
  ) |>
    relocate(split_id)
}

purge_training_set <- function(
  training_data,
  test_groups,
  training_groups,
  max_trade_periods,
  embargo
) {
  left_purge_groups <- training_groups[training_groups %in% (test_groups + 1)]
  right_purge_groups <- training_groups[training_groups %in% (test_groups - 1)]

  training_data |>
    filter(
      !(group_id %in%
        left_purge_groups &
        row_number() <= max_trade_periods + embargo),
      !(group_id %in%
        right_purge_groups &
        row_number() > n() - max_trade_periods),
      .by = group_id
    )
}

purge_training_sets <- function(
  data,
  max_trade_periods,
  embargo_pct = 0.01,
  purge_pct_threshold = 0.2
) {
  n_training_obs <- nrow(data$training_set[[1]])
  n_test_obs <- nrow(data$test_set[[1]])
  n_obs <- n_training_obs + n_test_obs

  embargo <- ceiling(n_obs * embargo_pct)

  purge <- partial(
    purge_training_set,
    max_trade_periods = max_trade_periods,
    embargo = embargo
  )

  purged_data <-
    data |>
    mutate(
      purged_training_set = pmap(
        list(training_set, test_groups, training_groups),
        \(ts, tg, trg) purge(ts, tg, trg)
      ),
      purged_pct = map_dbl(purged_training_set, \(pts) {
        (n_training_obs - nrow(pts)) / n_training_obs
      })
    )

  max_purged_pct <- max(purged_data$purged_pct)

  if (max_purged_pct > purge_pct_threshold) {
    cli::cli_warn(c(
      "Groups may be too small for reliable purging.",
      "i" = "In the worst-case split, you lost {scales::percent(max_purged_pct)} of training data.",
      "i" = "Threshold is {scales::percent(purge_pct_threshold)}.",
      "*" = "Try reducing n_groups or using more data."
    ))
  }

  purged_data
}
