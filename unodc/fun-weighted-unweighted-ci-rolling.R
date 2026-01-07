
library(data.table)

calc_percentage_ci2 <- function(
  dt,
  outcome_var,
  group_vars = NULL,
  weight_var = NULL,
  denominator_var = NULL,
  denominator_value = 1,
  na_treatment = "exclude",
  round_digits = 2,
  include_diagnostics = TRUE,
  include_ci = TRUE,
  ci_method = "wilson",
  # --- ### ROLLING: new params ---
  rolling_by = NULL,              # e.g., "year" (must be in group_vars)
  rolling_n  = NULL,              # e.g., 3 for 3-year window; NULL = off
  rolling_align = c("right", "center", "left"),
  rolling_method = c("sum_then_ratio", "mean_of_percent")
) {
  rolling_align  <- match.arg(rolling_align)
  rolling_method <- match.arg(rolling_method)

  # Input validation (fixed a couple of NULL checks)
  if (!is.data.table(dt)) stop("Input 'dt' must be a data.table")
  required_cols <- outcome_var
  if (!is.null(group_vars))      required_cols <- c(required_cols, group_vars)
  if (!is.null(weight_var))      required_cols <- c(required_cols, weight_var)
  if (!is.null(denominator_var)) required_cols <- c(required_cols, denominator_var)
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  if (!na_treatment %in% c("exclude", "as_zero")) stop("na_treatment must be 'exclude' or 'as_zero'")
  if (!ci_method %in% c("wilson", "normal"))      stop("ci_method must be 'wilson' or 'normal'")

  # --- ### ROLLING: validate rolling args
  if (!is.null(rolling_n)) {
    if (is.null(rolling_by)) stop("When using rolling_n, please set rolling_by (e.g., 'year').")
    if (is.null(group_vars) || !(rolling_by %in% group_vars)) {
      stop("rolling_by must be included in group_vars.")
    }
  }

  # Working copy
  dt_work <- copy(dt)

  # Filter denominator population if specified (fixed condition)
  if (!is.null(denominator_var)) {
    dt_work <- dt_work[get(denominator_var) == denominator_value]
    if (nrow(dt_work) == 0) {
      warning(paste("No rows found where", denominator_var, "==", denominator_value))
      return(data.table())
    }
  }

  # NA treatment → create outcome_indicator
  if (na_treatment == "exclude") {
    dt_work[, outcome_indicator := ifelse(is.na(get(outcome_var)), NA,
                                          ifelse(get(outcome_var) == 1, 1, 0))]
  } else {
    dt_work[, outcome_indicator := ifelse(get(outcome_var) == 1, 1, 0)]
    dt_work[is.na(outcome_indicator), outcome_indicator := 0]
  }

  # ------- Main aggregation (as in your original) -------
  if (is.null(weight_var)) {
    # UNWEIGHTED
    if (include_diagnostics) {
      if (is.null(group_vars)) {
        results <- dt_work[, .(
          numerator   = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          n_total     = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na= sum(is.na(get(outcome_var))),
          n_valid     = sum(!is.na(get(outcome_var))),
          calculation_type = "unweighted"
        )]
      } else {
        results <- dt_work[, .(
          numerator   = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          n_total     = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na= sum(is.na(get(outcome_var))),
          n_valid     = sum(!is.na(get(outcome_var))),
          calculation_type = "unweighted"
        ), by = group_vars]
      }
    } else {
      if (is.null(group_vars)) {
        results <- dt_work[, .(
          numerator   = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          calculation_type = "unweighted"
        )]
      } else {
        results <- dt_work[, .(
          numerator   = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          calculation_type = "unweighted"
        ), by = group_vars]
      }
    }
  } else {
    # WEIGHTED
    if (include_diagnostics) {
      if (is.null(group_vars)) {
        results <- dt_work[, .(
          numerator   = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
          sum_weights_squared = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
          n_total     = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na= sum(is.na(get(outcome_var))),
          n_valid     = sum(!is.na(get(outcome_var))),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        )]
      } else {
        results <- dt_work[, .(
          numerator   = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
          sum_weights_squared = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
          n_total     = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na= sum(is.na(get(outcome_var))),
          n_valid     = sum(!is.na(get(outcome_var))),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        ), by = group_vars]
      }
    } else {
      if (is.null(group_vars)) {
        results <- dt_work[, .(
          numerator   = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
          sum_weights_squared = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
          calculation_type = "weighted"
        )]
      } else {
        results <- dt_work[, .(
          numerator   = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]) else sum(get(weight_var)),
          sum_weights_squared = if (na_treatment == "exclude") sum(get(weight_var)[!is.na(outcome_indicator)]^2) else sum(get(weight_var)^2),
          calculation_type = "weighted"
        ), by = group_vars]
      }
    }
  }

  # Base percentage and proportion
  results[, percentage := (numerator / denominator) * 100]
  results[, proportion := numerator / denominator]

  # Confidence intervals for base series
  if (include_ci) {
    if (is.null(weight_var)) {
      if (ci_method == "wilson") {
        results[, c("ci_lower", "ci_upper") := {
          n <- denominator; p <- proportion; z <- qnorm(0.975)
          center <- (p + z^2/(2*n)) / (1 + z^2/n)
          margin <- z * sqrt((p * (1 - p) + z^2/(4*n)) / n) / (1 + z^2/n)
          list(pmax(0, (center - margin))*100, pmin(1, (center + margin))*100)
        }]
      } else {
        results[, c("ci_lower", "ci_upper") := {
          n <- denominator; p <- proportion
          se <- sqrt(p*(1-p)/n); m <- qnorm(0.975)*se
          list(pmax(0, p - m)*100, pmin(1, p + m)*100)
        }]
      }
    } else {
      results[, c("ci_lower", "ci_upper") := {
        p <- proportion; w_sum <- denominator; w2_sum <- sum_weights_squared
        se_w <- sqrt(p*(1 - p) * w2_sum / (w_sum^2))
        m <- qnorm(0.975) * se_w
        list(pmax(0, p - m)*100, pmin(1, p + m)*100)
      }]
    }
    results[, `:=`(ci_lower = round(ci_lower, round_digits),
                   ci_upper = round(ci_upper, round_digits))]
  }

  # --- ### ROLLING: compute rolling series BEFORE cleanup
  if (!is.null(rolling_n)) {
    # Helper: other groups (excluding the time var)
    other_groups <- setdiff(group_vars, rolling_by)

    # Ensure time var is ordered (numeric/date)
    # If it's character, try to convert to integer
    if (is.character(results[[rolling_by]])) {
      suppressWarnings(results[, (rolling_by) := as.integer(get(rolling_by))])
    }

    setorderv(results, c(other_groups, rolling_by), c(rep(1L, length(other_groups)), 1L))

    if (rolling_method == "sum_then_ratio") {
      # Roll numerator/denominator (and Σw² if weighted), then recompute percentage & CI
      if (is.null(weight_var)) {
        results[, `:=`(
          numerator_roll   = data.table::frollsum(numerator, n = rolling_n, align = rolling_align),
          denominator_roll = data.table::frollsum(denominator, n = rolling_n, align = rolling_align)
        ), by = other_groups]

        results[, proportion_roll := numerator_roll / denominator_roll]
        results[, percentage_roll := round(proportion_roll * 100, round_digits)]

        if (include_ci) {
          if (ci_method == "wilson") {
            results[, c("ci_lower_roll", "ci_upper_roll") := {
              n <- denominator_roll; p <- proportion_roll; z <- qnorm(0.975)
              center <- (p + z^2/(2*n)) / (1 + z^2/n)
              margin <- z * sqrt((p * (1 - p) + z^2/(4*n)) / n) / (1 + z^2/n)
              list(round(pmax(0, (center - margin))*100, round_digits),
                   round(pmin(1, (center + margin))*100, round_digits))
            }]
          } else {
            results[, c("ci_lower_roll", "ci_upper_roll") := {
              n <- denominator_roll; p <- proportion_roll
              se <- sqrt(p*(1-p)/n); m <- qnorm(0.975)*se
              list(round(pmax(0, p - m)*100, round_digits),
                   round(pmin(1, p + m)*100, round_digits))
            }]
          }
        }
      } else {
        # Weighted: roll numerator, denominator and sum_weights_squared
        results[, `:=`(
          numerator_roll   = data.table::frollsum(numerator, n = rolling_n, align = rolling_align),
          denominator_roll = data.table::frollsum(denominator, n = rolling_n, align = rolling_align),
          sum_weights_squared_roll = data.table::frollsum(sum_weights_squared, n = rolling_n, align = rolling_align)
        ), by = other_groups]

        results[, proportion_roll := numerator_roll / denominator_roll]
        results[, percentage_roll := round(proportion_roll * 100, round_digits)]

        if (include_ci) {
          results[, c("ci_lower_roll", "ci_upper_roll") := {
            p <- proportion_roll; w_sum <- denominator_roll; w2_sum <- sum_weights_squared_roll
            se_w <- sqrt(p*(1 - p) * w2_sum / (w_sum^2))
            m <- qnorm(0.975) * se_w
            list(round(pmax(0, p - m)*100, round_digits),
                 round(pmin(1, p + m)*100, round_digits))
          }]
        }
      }
    } else {
      # mean_of_percent: simple rolling mean of the already-computed percentage
      results[, percentage_roll := data.table::frollmean(percentage, n = rolling_n, align = rolling_align),
              by = other_groups]
      results[, percentage_roll := round(percentage_roll, round_digits)]
      # CI not strictly defined here; we skip by default
    }
  }

  # Round base percentage
  results[, percentage := round(percentage, round_digits)]

  # Drop rows where any grouping var is NA (fixed condition)
  if (!is.null(group_vars)) {
    for (var in group_vars) results <- results[!is.na(get(var))]
  }

  # Select columns
  if (include_diagnostics) {
    if (is.null(weight_var)) {
      base_cols <- c("percentage", "calculation_type", "n_total", "n_valid",
                     "n_outcome_1", "n_outcome_0", "n_outcome_na")
    } else {
      base_cols <- c("percentage", "calculation_type", "n_total", "n_valid",
                     "n_outcome_1", "n_outcome_0", "n_outcome_na", "sum_weights")
    }
  } else {
    base_cols <- c("percentage", "calculation_type")
  }
  if (include_ci) base_cols <- c(base_cols, "ci_lower", "ci_upper")

  # --- ### ROLLING: expose rolling columns if requested
  if (!is.null(rolling_n)) {
    roll_cols <- c("percentage_roll")
    if (rolling_method == "sum_then_ratio") {
      roll_cols <- c(roll_cols, "numerator_roll", "denominator_roll")
      if (!is.null(weight_var)) roll_cols <- c(roll_cols, "sum_weights_squared_roll")
      if (include_ci) roll_cols <- c(roll_cols, "ci_lower_roll", "ci_upper_roll")
    }
    base_cols <- c(base_cols, roll_cols)
  }

  final_cols <- if (is.null(group_vars)) base_cols else c(group_vars, base_cols)

  # Remove intermediate columns (keep roll intermediates already selected)
  cols_to_remove <- c("numerator", "denominator", "proportion",
                      "sum_weights_squared")  # safe if not present
  existing_rm <- intersect(cols_to_remove, names(results))
  if (length(existing_rm) > 0) results[, (existing_rm) := NULL]

  results <- results[, ..final_cols]
  return(results)
}
