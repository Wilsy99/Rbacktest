# ============================================================================
# metrics.R
# Backtesting Performance Metrics for Quantitative Strategy Validation
# ============================================================================
#
# Philosophy: All strategies are worthless random noise until proven otherwise
# with overwhelming statistical confidence. Every metric is an opportunity to
# reject your strategy—and most should be rejected.
#
# This is a defense-in-depth system where strategies must pass multiple,
# independent tests. Each metric catches a specific failure mode.
#
# References:
#   - López de Prado, "Advances in Financial Machine Learning" (DSR, PSR)
#   - Aronson, "Evidence-Based Technical Analysis" (Permutation testing)
#   - Lo, "The Statistics of Sharpe Ratios" (2002) (SR standard error)
#
# ============================================================================

# Dependencies ---------------------------------------------------------------
# Required packages: tibble, dplyr, purrr, ggplot2, moments
# Optional: furrr (for parallel permutation testing)

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
  library(purrr)
  library(ggplot2)
})

# ============================================================================
# CRITICAL IMPLEMENTATION RULES
# ============================================================================
#
# 1. NEVER pre-annualize returns before computing metrics
#    - Wrong:   sharpe <- mean(returns * 52) / sd(returns * 52)
#    - Correct: sharpe <- mean(returns) / sd(returns) * sqrt(52)
#
# 2. Distribution moments (skewness, kurtosis) are ALWAYS computed on
#    period-level returns, never annualized
#
# 3. Match periods_per_year to your rebalancing frequency:
#    - Daily:   252
#    - Weekly:  52
#    - Monthly: 12
#
# ============================================================================


# ============================================================================
# CORE METRICS
# ============================================================================

#' Compute Annualized Sharpe Ratio
#'
#' Calculates the Sharpe ratio using period returns and annualizes the result.
#' Never pre-annualizes returns—this preserves distribution properties.
#'
#' @param returns Numeric vector of period returns (not annualized)
#' @param periods_per_year Integer. 252 for daily, 52 for weekly, 12 for monthly
#' @param rf_annual Annual risk-free rate (default 0)
#'
#' @return Annualized Sharpe ratio (numeric scalar)
#'
#' @examples
#' weekly_returns <- c(0.01, -0.02, 0.015, 0.008, -0.005)
#' compute_sharpe(weekly_returns, periods_per_year = 52)
compute_sharpe <- function(returns, periods_per_year = 52, rf_annual = 0) {
  # Input validation
  if (length(returns) < 2) {
    warning("Sharpe ratio requires at least 2 observations")
    return(NA_real_)
  }
  
  if (any(is.na(returns))) {
    warning("NA values in returns. Removing for calculation.")
    returns <- returns[!is.na(returns)]
  }
  
  # Convert annual risk-free rate to period rate
  # Using simple division (standard practice for Sharpe)
  rf_period <- rf_annual / periods_per_year
  
  excess_returns <- returns - rf_period
  
  # Compute ratio on period returns, THEN annualize
  # This is the correct order of operations
  mean(excess_returns) / sd(excess_returns) * sqrt(periods_per_year)
}


#' Compute Sharpe Ratio Standard Error (Andrew Lo, 2002)
#'
#' Calculates the standard error of the Sharpe ratio estimate accounting for

' non-Normal returns (skewness and kurtosis).
#'
#' @param returns Numeric vector of period returns
#' @param sr Annualized Sharpe ratio (pre-computed)
#'
#' @return Standard error of Sharpe estimate (numeric scalar)
#'
#' @details
#' Formula from Lo (2002) for non-Normal returns:
#' SE(SR) = sqrt((1 - skew*SR + (kurt-1)/4 * SR^2) / (n-1))
#'
#' This generalizes the Normal case where skew=0, kurt=3, giving:
#' SE(SR) = sqrt((1 + SR^2/2) / (n-1))
compute_sharpe_se <- function(returns, sr) {
  n <- length(returns)
  
  if (n < 3) {
    warning("Need at least 3 observations for reliable SE estimate")
    return(NA_real_)
  }
  
  # Higher moments from PERIOD returns (never annualized)
  skew <- moments::skewness(returns)
  kurt <- moments::kurtosis(returns)  # Raw kurtosis (Gaussian = 3)
  

  # Andrew Lo (2002) formula for non-Normal returns
  sqrt((1 - skew * sr + (kurt - 1) / 4 * sr^2) / (n - 1))
}


#' Compute Deflated Sharpe Ratio (DSR)
#'
#' Adjusts Sharpe ratio for multiple testing / selection bias. When you test
#' N strategy variants and report the best, you're cherry-picking. DSR
#' increases the hurdle rate based on how many trials you ran.
#'
#' @param returns Numeric vector of period returns
#' @param n_trials Number of strategy variants tested (default 1)
#' @param periods_per_year Annualization factor (default 52 for weekly)
#' @param rf_annual Annual risk-free rate (default 0)
#'
#' @return DSR z-score. Interpretation:
#'   - DSR > 1.65: 95% confidence (minimum for statistical significance)
#'   - DSR > 2.00: 97.5% confidence (recommended for real money)
#'   - DSR > 0: Only 50% confidence (coin flip—NOT significant)
#'
#' @details
#' DSR is a z-score: (Sharpe - threshold) / SE
#' where threshold = sqrt(log(n_trials) / (n-1))
#'
#' The threshold increases with sqrt(log(n_trials)), penalizing data snooping.
#'
#' @examples
#' # Single strategy test
#' compute_dsr(returns, n_trials = 1)
#'
#' # After testing 100 parameter combinations
#' compute_dsr(returns, n_trials = 100)
compute_dsr <- function(returns, n_trials = 1, periods_per_year = 52, rf_annual = 0) {
  # Input validation
  if (n_trials < 1) {
    stop("n_trials must be at least 1")
  }
  
  n <- length(returns)
  if (n < 3) {
    warning("DSR requires at least 3 observations")
    return(NA_real_)
  }
  
  # Compute annualized Sharpe
  sr <- compute_sharpe(returns, periods_per_year, rf_annual)
  
  # Standard error (Lo, 2002)
  sr_se <- compute_sharpe_se(returns, sr)
  
  # Selection bias threshold (López de Prado)
  # Increases with number of trials tested
  threshold <- sqrt(log(n_trials) / (n - 1))
  
  # DSR = z-score of adjusted Sharpe
  (sr - threshold) / sr_se
}


#' Compute Probabilistic Sharpe Ratio (PSR)
#'
#' Calculates the probability that the true Sharpe ratio exceeds a benchmark,
#' given estimation uncertainty. Your observed Sharpe is a noisy estimate—
#' PSR quantifies your confidence it's real.
#'
#' @param returns Numeric vector of period returns
#' @param benchmark_sr Benchmark Sharpe to beat (default 0)
#' @param periods_per_year Annualization factor (default 52)
#' @param rf_annual Annual risk-free rate (default 0)
#'
#' @return Probability (0-1) that true Sharpe > benchmark_sr
#'   - PSR > 0.95: Pass (95% confident true Sharpe is positive)
#'   - PSR < 0.95: Fail (too much estimation uncertainty)
#'
#' @examples
#' # Is true Sharpe likely positive?
#' compute_psr(returns, benchmark_sr = 0)
#'
#' # Does it beat a 0.5 Sharpe benchmark?
#' compute_psr(returns, benchmark_sr = 0.5)
compute_psr <- function(returns, benchmark_sr = 0, periods_per_year = 52, rf_annual = 0) {
  n <- length(returns)
  if (n < 3) {
    warning("PSR requires at least 3 observations")
    return(NA_real_)
  }
  
  # Compute annualized Sharpe
  sr <- compute_sharpe(returns, periods_per_year, rf_annual)
  
  # Standard error (Lo, 2002)
  sr_se <- compute_sharpe_se(returns, sr)
  
  # PSR = P(true_SR > benchmark | observed data)
  # Using Normal approximation
  pnorm((sr - benchmark_sr) / sr_se)
}


#' Compute Maximum Drawdown (Depth and Duration)
#'
#' Tracks both the depth (largest peak-to-trough loss) and duration
#' (time to recover) of drawdowns. A 40% drawdown requires 67% gain
#' to recover—most traders can't survive it.
#'
#' @param equity_curve Numeric vector of cumulative wealth (starts at 1)
#'
#' @return List with:
#'   - depth: Maximum drawdown as negative decimal (e.g., -0.25 = 25% loss)
#'   - duration: Periods in longest underwater stretch
#'   - drawdown_tbl: Full drawdown tibble for analysis
#'
#' @details
#' Pass conditions (typical):
#'   - Depth: > -0.20 (-20%)
#'   - Duration: < 104 periods (2 years for weekly data)
compute_max_dd <- function(equity_curve) {
  if (length(equity_curve) < 2) {
    warning("Max DD requires at least 2 observations")
    return(list(depth = NA_real_, duration = NA_integer_, drawdown_tbl = NULL))
  }
  
  # Build drawdown table
  dd_tbl <- tibble(
    period = seq_along(equity_curve),
    equity = equity_curve
  ) |>
    mutate(
      running_max = cummax(equity),
      drawdown = (equity - running_max) / running_max,
      underwater = running_max > equity
    )
  
  # Maximum depth
  depth <- min(dd_tbl$drawdown)
  
  # Duration: longest consecutive underwater period
  # Use run-length encoding for efficiency
  if (any(dd_tbl$underwater)) {
    duration <- dd_tbl |>
      mutate(
        # Create group IDs for consecutive underwater periods
        new_dd = !underwater & lag(underwater, default = FALSE),
        dd_id = cumsum(new_dd | (!underwater & !lag(underwater, default = TRUE)))
      ) |>
      filter(underwater) |>
      count(dd_id) |>
      pull(n) |>
      max()
  } else {
    duration <- 0L
  }
  
  list(
    depth = depth,
    duration = duration,
    drawdown_tbl = dd_tbl
  )
}


#' Compute Calmar Ratio
#'
#' CAGR divided by maximum drawdown depth. Explicitly penalizes tail risk
#' that Sharpe can hide with occasional huge wins.
#'
#' @param returns Numeric vector of period returns
#' @param equity_curve Numeric vector of cumulative wealth (optional—computed if NULL)
#' @param periods_per_year Annualization factor (default 52)
#'
#' @return Calmar ratio (numeric scalar)
#'   - Calmar > 0.5: Decent
#'   - Calmar > 1.0: Excellent
#'   - Calmar > 2.0: Suspicious (investigate for overfitting)
compute_calmar <- function(returns, equity_curve = NULL, periods_per_year = 52) {
  if (length(returns) < 2) {
    warning("Calmar requires at least 2 observations")
    return(NA_real_)
  }
  
  # Build equity curve if not provided
  if (is.null(equity_curve)) {
    equity_curve <- cumprod(1 + returns)
  }
  
  # CAGR (Compound Annual Growth Rate)
  n_years <- length(returns) / periods_per_year
  total_return <- tail(equity_curve, 1) - 1
  cagr <- (1 + total_return)^(1 / n_years) - 1
  
  # Maximum drawdown depth
  max_dd_depth <- compute_max_dd(equity_curve)$depth
  
  # Handle edge case: no drawdown

  if (max_dd_depth == 0) {
    warning("No drawdown detected. Calmar undefined (returning Inf).")
    return(Inf)
  }
  
  cagr / abs(max_dd_depth)
}


# ============================================================================
# PERMUTATION TESTING: THE LUCK TEST
# ============================================================================

#' Permutation Test for Strategy Validation
#'
#' The "Final Boss" of validation. Shuffles returns to destroy temporal
#' structure while preserving the marginal distribution, then tests if
#' real performance beats random chance.
#'
#' @param returns Numeric vector of period returns
#' @param n_perms Number of permutations (default 1000; use 10000 for publication)
#' @param periods_per_year Annualization factor (default 52)
#' @param test_stat Which statistic to test: "sharpe" or "total_return"
#' @param seed Random seed for reproducibility (default NULL)
#'
#' @return Tibble with:
#'   - observed: Your strategy's test statistic
#'   - p_value: Proportion of permutations >= observed (one-tailed)
#'   - perm_mean, perm_sd: Permutation distribution summary
#'   - perm_95, perm_99: Threshold percentiles
#'   - permuted: List-column with full permutation distribution
#'
#' @details
#' Pass conditions:
#'   - p_value < 0.05: Beats 95% of shuffles (minimum)
#'   - p_value < 0.01: Beats 99% of shuffles (recommended)
#'
#' If your strategy fails this test, it IS noise—stop analysis immediately.
#'
#' @examples
#' perm_result <- permutation_test(returns, n_perms = 1000)
#' if (perm_result$p_value > 0.05) {
#'   message("Strategy fails luck test. REJECT.")
#' }
permutation_test <- function(returns,
                              n_perms = 1000,
                              periods_per_year = 52,
                              test_stat = c("sharpe", "total_return"),
                              seed = NULL) {
  
  test_stat <- match.arg(test_stat)
  
  if (length(returns) < 10) {
    warning("Permutation test unreliable with < 10 observations")
  }
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Define test statistic function
  compute_stat <- function(ret) {
    switch(test_stat,
      sharpe = compute_sharpe(ret, periods_per_year),
      total_return = prod(1 + ret) - 1
    )
  }
  
  # Observed statistic
  observed <- compute_stat(returns)
  
  # Generate permutation distribution
  # Using purrr::map_dbl for clarity; use furrr::future_map_dbl for parallelism
  permuted <- map_dbl(seq_len(n_perms), \(i) {
    shuffled <- sample(returns)  # Destroy temporal structure
    compute_stat(shuffled)
  })
  
  # Compute p-value (one-tailed: proportion >= observed)
  p_value <- mean(permuted >= observed)
  
  tibble(
    test_statistic = test_stat,
    observed = observed,
    p_value = p_value,
    perm_mean = mean(permuted),
    perm_sd = sd(permuted),
    perm_95 = quantile(permuted, 0.95),
    perm_99 = quantile(permuted, 0.99),
    n_perms = n_perms,
    permuted = list(permuted)  # Store for plotting
  )
}


#' Plot Permutation Test Results
#'
#' Visualizes the "luck factor"—if your red line is barely past the grey blob,
#' you're not special, you're lucky.
#'
#' @param perm_result Result from permutation_test()
#'
#' @return ggplot object
plot_permutation <- function(perm_result) {
  # Extract permutation distribution
  perm_dist <- tibble(value = perm_result$permuted[[1]])
  
  # Determine x-axis label based on test statistic
  x_label <- switch(perm_result$test_statistic,
    sharpe = "Sharpe Ratio",
    total_return = "Total Return",
    "Test Statistic"
  )
  
  ggplot(perm_dist, aes(x = value)) +
    geom_histogram(
      bins = 30,
      fill = "grey70",
      color = "white",
      alpha = 0.8
    ) +
    # Your strategy (red)
    geom_vline(
      xintercept = perm_result$observed,
      color = "#E41A1C",
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    # 95th percentile threshold (blue)
    geom_vline(
      xintercept = perm_result$perm_95,
      color = "#377EB8",
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    # Annotation
    annotate(
      "text",
      x = perm_result$observed,
      y = Inf,
      label = sprintf("Your Strategy\n(p = %.3f)", perm_result$p_value),
      vjust = 1.5,
      hjust = if (perm_result$observed > perm_result$perm_mean) 1.1 else -0.1,
      color = "#E41A1C",
      size = 3.5,
      fontface = "bold"
    ) +
    labs(
      title = "The Luck Test: Are You Special or Just Lucky?",
      subtitle = sprintf(
        "Red = Your strategy | Grey = %s shuffled versions | Blue dashed = 95th percentile",
        format(perm_result$n_perms, big.mark = ",")
      ),
      x = x_label,
      y = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )
}


# ============================================================================
# MASTER METRICS FUNCTION
# ============================================================================

#' Compute Complete Performance Metrics Battery
#'
#' The main entry point for strategy validation. Computes all metrics
#' and returns pass/fail flags for automated screening.
#'
#' @param returns_tbl Tibble with a `strategy_return` column, or numeric vector
#' @param periods_per_year Annualization factor (default 52 for weekly)
#' @param n_trials Number of strategy variants tested (for DSR adjustment)
#' @param rf_annual Annual risk-free rate (default 0)
#' @param run_permutation Whether to run permutation test (default TRUE)
#' @param n_perms Number of permutations if running (default 1000)
#' @param seed Random seed for permutation test reproducibility
#'
#' @return Tibble with all metrics and pass/fail flags
#'
#' @details
#' Pass/fail thresholds (institutional grade):
#'   - PSR > 0.95 (95% confident true Sharpe is positive)
#'   - DSR > 1.65 (95% confidence after multiple testing adjustment)
#'   - Permutation p-value < 0.05 (beats 95% of random shuffles)
#'
#' @examples
#' strategy_returns_tbl |>
#'   compute_performance_metrics(
#'     periods_per_year = 52,
#'     n_trials = 1,
#'     rf_annual = 0.04
#'   )
compute_performance_metrics <- function(returns_tbl,
                                         periods_per_year = 52,
                                         n_trials = 1,
                                         rf_annual = 0,
                                         run_permutation = TRUE,
                                         n_perms = 1000,
                                         seed = NULL) {
  
  # Extract returns vector (handle tibble or vector input)
  if (is.data.frame(returns_tbl)) {
    if (!"strategy_return" %in% names(returns_tbl)) {
      stop("returns_tbl must have a 'strategy_return' column")
    }
    returns <- returns_tbl$strategy_return
  } else if (is.numeric(returns_tbl)) {
    returns <- returns_tbl
  } else {
    stop("returns_tbl must be a tibble with 'strategy_return' column or numeric vector")
  }
  
  # Remove NAs with warning
  if (any(is.na(returns))) {
    n_na <- sum(is.na(returns))
    warning(sprintf("Removing %d NA values from returns", n_na))
    returns <- returns[!is.na(returns)]
  }
  
  n <- length(returns)
  if (n < 10) {
    warning("Very few observations (", n, "). Results will be unreliable.")
  }
  
  # Build equity curve
  equity <- cumprod(1 + returns)
  
  # Compute individual metrics
  sharpe_val <- compute_sharpe(returns, periods_per_year, rf_annual)
  dsr_val <- compute_dsr(returns, n_trials, periods_per_year, rf_annual)
  psr_val <- compute_psr(returns, benchmark_sr = 0, periods_per_year, rf_annual)
  calmar_val <- compute_calmar(returns, equity, periods_per_year)
  max_dd_result <- compute_max_dd(equity)
  
  # Permutation test (optional but recommended)
  if (run_permutation) {
    perm_result <- permutation_test(
      returns,
      n_perms = n_perms,
      periods_per_year = periods_per_year,
      test_stat = "sharpe",
      seed = seed
    )
    perm_pval <- perm_result$p_value
  } else {
    perm_pval <- NA_real_
  }
  
  # Assemble results
  tibble(
    # Sample information
    n_periods = n,
    periods_per_year = periods_per_year,
    n_years = n / periods_per_year,
    n_trials = n_trials,
    
    # Return metrics
    total_return = tail(equity, 1) - 1,
    cagr = (tail(equity, 1))^(periods_per_year / n) - 1,
    
    # Risk metrics
    volatility_ann = sd(returns) * sqrt(periods_per_year),
    
    # Risk-adjusted metrics
    sharpe = sharpe_val,
    deflated_sharpe = dsr_val,
    prob_sharpe = psr_val,
    calmar = calmar_val,
    
    # Drawdown metrics
    max_dd_depth = max_dd_result$depth,
    max_dd_duration = max_dd_result$duration,
    
    # Luck test
    perm_pval = perm_pval,
    
    # Pass/Fail flags (institutional thresholds)
    pass_psr = psr_val > 0.95,
    pass_dsr = dsr_val > 1.65,
    pass_perm = if (!is.na(perm_pval)) perm_pval < 0.05 else NA,
    
    # Overall verdict
    pass_all = pass_psr & pass_dsr & (is.na(pass_perm) | pass_perm)
  )
}


# ============================================================================
# GATEKEEPING FUNCTION
# ============================================================================

#' Quick Validation Gate (Permutation-First Strategy)
#'
#' Use this as the first filter before expensive analysis. If a strategy
#' doesn't beat shuffled data, it IS noise—don't waste time on it.
#'
#' @param returns Numeric vector of period returns
#' @param n_perms Number of permutations (default 1000)
#' @param periods_per_year Annualization factor (default 52)
#' @param alpha Significance level (default 0.05)
#' @param verbose Print result message (default TRUE)
#'
#' @return List with:
#'   - pass: Logical indicating if strategy passed
#'   - perm_result: Full permutation test result
#'   - message: Human-readable verdict
#'
#' @examples
#' gate_result <- validate_strategy_gate(returns)
#' if (!gate_result$pass) {
#'   stop("Strategy rejected at gate. Analysis terminated.")
#' }
validate_strategy_gate <- function(returns,
                                    n_perms = 1000,
                                    periods_per_year = 52,
                                    alpha = 0.05,
                                    verbose = TRUE) {
  
  perm_result <- permutation_test(
    returns,
    n_perms = n_perms,
    periods_per_year = periods_per_year,
    test_stat = "sharpe"
  )
  
  pass <- perm_result$p_value < alpha
  
  message <- if (pass) {
    sprintf(
      "PASS: Strategy beats %.0f%% of random shuffles (p = %.4f). Proceed to full validation.",
      (1 - perm_result$p_value) * 100,
      perm_result$p_value
    )
  } else {
    sprintf(
      "REJECT: Strategy fails luck test (p = %.4f). Performance likely due to random chance.",
      perm_result$p_value
    )
  }
  
  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat(" VALIDATION GATE: Permutation Test\n")
    cat(strrep("=", 60), "\n", sep = "")
    cat(" Observed Sharpe: ", sprintf("%.3f", perm_result$observed), "\n", sep = "")
    cat(" 95th percentile: ", sprintf("%.3f", perm_result$perm_95), "\n", sep = "")
    cat(" p-value:         ", sprintf("%.4f", perm_result$p_value), "\n", sep = "")
    cat(strrep("-", 60), "\n", sep = "")
    cat(" ", message, "\n", sep = "")
    cat(strrep("=", 60), "\n\n", sep = "")
  }
  
  list(
    pass = pass,
    perm_result = perm_result,
    message = message
  )
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Format Metrics for Display
#'
#' Pretty-prints a metrics tibble with appropriate formatting.
#'
#' @param metrics_tbl Result from compute_performance_metrics()
#'
#' @return Formatted tibble for display
format_metrics <- function(metrics_tbl) {
  metrics_tbl |>
    mutate(
      across(c(total_return, cagr, volatility_ann, max_dd_depth), ~ scales::percent(.x, accuracy = 0.1)),
      across(c(sharpe, deflated_sharpe, calmar), ~ sprintf("%.2f", .x)),
      prob_sharpe = scales::percent(prob_sharpe, accuracy = 0.1),
      perm_pval = if_else(is.na(perm_pval), "Not run", sprintf("%.4f", perm_pval)),
      across(starts_with("pass_"), ~ if_else(.x, "✓ PASS", "✗ FAIL", missing = "—"))
    )
}


#' Print Validation Summary
#'
#' Prints a human-readable validation summary for a strategy.
#'
#' @param metrics_tbl Result from compute_performance_metrics()
#' @param strategy_name Optional name for the strategy
print_validation_summary <- function(metrics_tbl, strategy_name = "Strategy") {
  m <- metrics_tbl
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf(" VALIDATION SUMMARY: %s\n", strategy_name))
  cat(strrep("=", 70), "\n\n")
  
  cat(" SAMPLE\n")
  cat(sprintf("   Periods: %d (%.1f years)\n", m$n_periods, m$n_years))
  cat(sprintf("   Trials tested: %d\n", m$n_trials))
  cat("\n")
  
  cat(" RETURNS\n")
  cat(sprintf("   Total Return: %+.1f%%\n", m$total_return * 100))
  cat(sprintf("   CAGR:         %+.1f%%\n", m$cagr * 100))
  cat(sprintf("   Volatility:   %.1f%% (annualized)\n", m$volatility_ann * 100))
  cat("\n")
  
  cat(" RISK-ADJUSTED\n")
  cat(sprintf("   Sharpe:           %.2f\n", m$sharpe))
  cat(sprintf("   Deflated Sharpe:  %.2f  %s\n", m$deflated_sharpe,
              if (m$pass_dsr) "[PASS > 1.65]" else "[FAIL]"))
  cat(sprintf("   Probabilistic SR: %.1f%% %s\n", m$prob_sharpe * 100,
              if (m$pass_psr) "[PASS > 95%]" else "[FAIL]"))
  cat(sprintf("   Calmar:           %.2f\n", m$calmar))
  cat("\n")
  
  cat(" DRAWDOWN\n")
  cat(sprintf("   Max Depth:    %.1f%%\n", m$max_dd_depth * 100))
  cat(sprintf("   Max Duration: %d periods\n", m$max_dd_duration))
  cat("\n")
  
  cat(" LUCK TEST\n")
  if (!is.na(m$perm_pval)) {
    cat(sprintf("   p-value: %.4f  %s\n", m$perm_pval,
                if (m$pass_perm) "[PASS < 0.05]" else "[FAIL]"))
  } else {
    cat("   Not run\n")
  }
  cat("\n")
  
  cat(strrep("-", 70), "\n")
  verdict <- if (m$pass_all) {
    " VERDICT: PASS — Strategy survives statistical validation"
  } else {
    " VERDICT: FAIL — Strategy does not meet institutional standards"
  }
  cat(verdict, "\n")
  cat(strrep("=", 70), "\n\n")
}


