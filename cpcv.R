ntile <- function(x, n) {
  len <- length(x)
  if (len == 0) {
    return(numeric(0))
  }

  as.integer(((1:len - 1) * n) / len) + 1
}

generate_test_set <- function(
  data,
  test_groups
) {
  data |>
    fsubset(group_id %in% test_groups)
}

generate_purged_training_set <- function(
  data,
  test_groups,
  training_groups,
  max_trade_periods,
  embargo_pct
) {
  embargo <- ceiling(fnrow(data) * embargo_pct)

  purge_start_groups <- training_groups[training_groups %in% (test_groups + 1)]
  purge_end_groups <- training_groups[training_groups %in% (test_groups - 1)]

  data |>
    fsubset(
      group_id %in%
        training_groups &
        !(group_id %in%
          purge_start_groups &
          group_row_id <= max_trade_periods + embargo) &
        !(group_id %in%
          purge_end_groups &
          group_row_id > group_n_obs - max_trade_periods)
    )
}

generate_test_set_factory <- function(
  test_groups
) {
  function(data) {
    generate_test_set(data = data, test_groups = test_groups)
  }
}

generate_purged_training_set_factory <- function(
  test_groups,
  training_groups,
  max_trade_periods,
  embargo_pct
) {
  function(data) {
    generate_purged_training_set(
      data = data,
      test_groups = test_groups,
      training_groups = training_groups,
      max_trade_periods = max_trade_periods,
      embargo_pct = embargo_pct
    )
  }
}

generate_splits <- function(
  data,
  n_groups,
  n_test_groups,
  max_trade_periods,
  embargo_pct
) {
  data_with_groups <-
    data |>
    fmutate(
      group_id = ntile(x = seq_row(data), n = n_groups),
      group_row_id = seqid(group_id),
      group_n_obs = fnobs(group_id)
    )

  split_config <-
    tibble(
      split_id = 1:choose(n_groups, n_test_groups),
      test_groups = combn(n_groups, n_test_groups, simplify = FALSE),
      training_groups = map(test_groups, \(test_group) {
        setdiff(1:n_groups, test_group)
      }),
      generate_test_set_fn = map(test_groups, \(tg) {
        generate_test_set_factory(tg)
      }),
      generate_purged_training_set_fn = map2(
        test_groups,
        training_groups,
        \(tg, trg) {
          generate_purged_training_set_factory(
            tg,
            trg,
            max_trade_periods,
            embargo_pct
          )
        }
      )
    )

  list(
    data = data_with_groups,
    split_config = split_config
  )
}

# purge_training_set <- function(
#   training_data,
#   test_groups,
#   training_groups,
#   max_trade_periods,
#   embargo
# ) {
#   left_purge_groups <- training_groups[training_groups %in% (test_groups + 1)]
#   right_purge_groups <- training_groups[training_groups %in% (test_groups - 1)]

#   training_data |>
#     filter(
#       !(group_id %in%
#         left_purge_groups &
#         row_number() <= max_trade_periods + embargo),
#       !(group_id %in%
#         right_purge_groups &
#         row_number() > n() - max_trade_periods),
#       .by = group_id
#     )
# }

# purge_training_sets <- function(
#   data,
#   max_trade_periods,
#   embargo_pct = 0.01,
#   purge_pct_threshold = 0.2
# ) {
#   n_training_obs <- nrow(data$training_set[[1]])
#   n_test_obs <- nrow(data$test_set[[1]])
#   n_obs <- n_training_obs + n_test_obs

#   embargo <- ceiling(n_obs * embargo_pct)

#   purge <- partial(
#     purge_training_set,
#     max_trade_periods = max_trade_periods,
#     embargo = embargo
#   )

#   purged_data <-
#     data |>
#     mutate(
#       purged_training_set = pmap(
#         list(training_set, test_groups, training_groups),
#         \(ts, tg, trg) purge(ts, tg, trg)
#       ),
#       purged_pct = map_dbl(purged_training_set, \(pts) {
#         (n_training_obs - nrow(pts)) / n_training_obs
#       })
#     )

#   max_purged_pct <- max(purged_data$purged_pct)

#   if (max_purged_pct > purge_pct_threshold) {
#     cli::cli_warn(c(
#       "Groups may be too small for reliable purging.",
#       "i" = "In the worst-case split, you lost {scales::percent(max_purged_pct)} of training data.",
#       "i" = "Threshold is {scales::percent(purge_pct_threshold)}.",
#       "*" = "Try reducing n_groups or using more data."
#     ))
#   }

#   purged_data
# }
