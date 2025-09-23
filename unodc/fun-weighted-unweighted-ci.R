library(data.table)

#' Calculate Weighted or Unweighted Percentages by Group or Overall with 95% Confidence Intervals
#'
#' This function calculates percentages of a binary outcome variable either overall
#' or by one or more grouping variables. It can perform both weighted and unweighted
#' calculations depending on whether a weight variable is provided. Includes 95% confidence intervals.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the binary outcome variable (should be 0/1 or logical)
#' @param group_vars Character vector. Names of grouping variables (optional). If NULL, calculates overall percentage
#' @param weight_var Character string. Name of the weight variable (optional). If NULL, unweighted percentages are calculated
#' @param denominator_var Character string. Name of variable defining the denominator population (optional)
#' @param denominator_value Numeric. Value of denominator_var to include in calculations (default: 1)
#' @param na_treatment Character. How to treat NA values in outcome_var: "exclude" (default) or "as_zero"
#' @param round_digits Integer. Number of decimal places for percentage (default: 2)
#' @param include_diagnostics Logical. Whether to include diagnostic columns (default: TRUE)
#' @param include_ci Logical. Whether to include 95% confidence intervals (default: TRUE)
#' @param ci_method Character. Method for CI calculation: "wilson" (default) for unweighted, "normal" for weighted
#'
#' @return A data.table with grouping variables, percentages, confidence intervals, and optional diagnostic information
#'
#' @details
#' ## Weighted Percentage (when weight_var is provided):
#' Weighted Percentage = (Σ(weight × I(outcome = 1))) / (Σ(weight)) × 100
#'
#' For weighted confidence intervals, uses normal approximation with design-adjusted standard error:
#' SE = sqrt(p * (1 - p) * Σ(w²) / (Σ(w))²)
#'
#' ## Unweighted Percentage (when weight_var is NULL):
#' Unweighted Percentage = (Count(outcome = 1)) / (Count(total)) × 100
#'
#' For unweighted confidence intervals, uses Wilson score interval (default) or normal approximation.
#'
#' Where I(outcome = 1) is an indicator function that equals 1 when outcome = 1, 0 otherwise.
#'
#' When a denominator variable is specified, only rows where denominator_var == denominator_value
#' are included in the calculation.
#'
#' @examples
#' # Overall weighted percentage with CI
#' calc_percentage_ci(dt, "ltp_cannabis", weight_var = "VEKT", denominator_var = "canpop")
#'
#' # Weighted percentage by group with CI
#' calc_percentage_ci(dt, "ltp_cannabis", "agecat", weight_var = "VEKT", denominator_var = "canpop")
#'
#' @author Your Name
#' @export
calc_percentage_ci <- function(dt,
                           outcome_var,
                           group_vars = NULL,
                           weight_var = NULL,
                           denominator_var = NULL,
                           denominator_value = 1,
                           na_treatment = "exclude",
                           round_digits = 2,
                           include_diagnostics = TRUE,
                           include_ci = TRUE,
                           ci_method = "wilson") {

  # Input validation
  if (!is.data.table(dt)) {
    stop("Input 'dt' must be a data.table")
  }

  required_cols <- outcome_var
  if (!is.null(group_vars)) {
    required_cols <- c(required_cols, group_vars)
  }
  if (!is.null(weight_var)) {
    required_cols <- c(required_cols, weight_var)
  }
  if (!is.null(denominator_var)) {
    required_cols <- c(required_cols, denominator_var)
  }

  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!na_treatment %in% c("exclude", "as_zero")) {
    stop("na_treatment must be 'exclude' or 'as_zero'")
  }

  if (!ci_method %in% c("wilson", "normal")) {
    stop("ci_method must be 'wilson' or 'normal'")
  }

  # Create working copy
  dt_work <- copy(dt)

  # Filter to denominator population if specified
  if (!is.null(denominator_var)) {
    dt_work <- dt_work[get(denominator_var) == denominator_value]

    if (nrow(dt_work) == 0) {
      warning(paste("No rows found where", denominator_var, "==", denominator_value))
      return(data.table())
    }
  }

  # Handle NA treatment in outcome variable
  if (na_treatment == "exclude") {
    # Create indicator: 1 if outcome == 1, 0 if outcome == 0, NA if outcome is NA
    dt_work[, outcome_indicator := ifelse(is.na(get(outcome_var)), NA,
                                        ifelse(get(outcome_var) == 1, 1, 0))]
  } else if (na_treatment == "as_zero") {
    # Create indicator: 1 if outcome == 1, 0 otherwise (including NA)
    dt_work[, outcome_indicator := ifelse(get(outcome_var) == 1, 1, 0)]
    dt_work[is.na(outcome_indicator), outcome_indicator := 0]
  }

  # Calculate percentages - overall or by group - weighted or unweighted
  if (is.null(weight_var)) {
    # UNWEIGHTED CALCULATION
    if (include_diagnostics) {
      if (is.null(group_vars)) {
        # Overall calculation (no grouping)
        results <- dt_work[, .(
          # Numerator: count where outcome = 1
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),

          # Denominator: total count (excluding NAs in outcome if na_treatment == "exclude")
          denominator = if (na_treatment == "exclude") {
            sum(!is.na(outcome_indicator))
          } else {
            .N
          },

          # Diagnostic information
          n_total = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na = sum(is.na(get(outcome_var))),
          n_valid = sum(!is.na(get(outcome_var))),
          calculation_type = "unweighted"
        )]
      } else {
        # Grouped calculation
        results <- dt_work[, .(
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(!is.na(outcome_indicator))
          } else {
            .N
          },
          n_total = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na = sum(is.na(get(outcome_var))),
          n_valid = sum(!is.na(get(outcome_var))),
          calculation_type = "unweighted"
        ), by = group_vars]
      }
    } else {
      if (is.null(group_vars)) {
        # Overall calculation (no grouping)
        results <- dt_work[, .(
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(!is.na(outcome_indicator))
          } else {
            .N
          },
          calculation_type = "unweighted"
        )]
      } else {
        # Grouped calculation
        results <- dt_work[, .(
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(!is.na(outcome_indicator))
          } else {
            .N
          },
          calculation_type = "unweighted"
        ), by = group_vars]
      }
    }

  } else {
    # WEIGHTED CALCULATION
    if (include_diagnostics) {
      if (is.null(group_vars)) {
        # Overall calculation (no grouping)
        results <- dt_work[, .(
          # Numerator: weighted sum where outcome = 1
          numerator = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),

          # Denominator: total weighted sum (excluding NAs in outcome if na_treatment == "exclude")
          denominator = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)])
          } else {
            sum(get(weight_var))
          },

          # Sum of squared weights (needed for weighted CI)
          sum_weights_squared = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)]^2)
          } else {
            sum(get(weight_var)^2)
          },

          # Diagnostic information
          n_total = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na = sum(is.na(get(outcome_var))),
          n_valid = sum(!is.na(get(outcome_var))),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        )]
      } else {
        # Grouped calculation
        results <- dt_work[, .(
          numerator = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)])
          } else {
            sum(get(weight_var))
          },
          sum_weights_squared = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)]^2)
          } else {
            sum(get(weight_var)^2)
          },
          n_total = .N,
          n_outcome_1 = sum(get(outcome_var) == 1, na.rm = TRUE),
          n_outcome_0 = sum(get(outcome_var) == 0, na.rm = TRUE),
          n_outcome_na = sum(is.na(get(outcome_var))),
          n_valid = sum(!is.na(get(outcome_var))),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        ), by = group_vars]
      }
    } else {
      if (is.null(group_vars)) {
        # Overall calculation (no grouping)
        results <- dt_work[, .(
          numerator = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)])
          } else {
            sum(get(weight_var))
          },
          sum_weights_squared = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)]^2)
          } else {
            sum(get(weight_var)^2)
          },
          calculation_type = "weighted"
        )]
      } else {
        # Grouped calculation
        results <- dt_work[, .(
          numerator = sum(get(weight_var) * outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)])
          } else {
            sum(get(weight_var))
          },
          sum_weights_squared = if (na_treatment == "exclude") {
            sum(get(weight_var)[!is.na(outcome_indicator)]^2)
          } else {
            sum(get(weight_var)^2)
          },
          calculation_type = "weighted"
        ), by = group_vars]
      }
    }
  }

  # Calculate percentage and proportion
  results[, percentage := (numerator / denominator) * 100]
  results[, proportion := numerator / denominator]

  # Calculate 95% confidence intervals
  if (include_ci) {
    if (is.null(weight_var)) {
      # UNWEIGHTED CONFIDENCE INTERVALS
      if (ci_method == "wilson") {
        # Wilson score interval (more accurate for small samples and extreme proportions)
        results[, c("ci_lower", "ci_upper") := {
          n <- denominator
          p <- proportion
          z <- qnorm(0.975)  # 95% CI

          # Wilson interval
          center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
          margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)

          ci_lower_prop <- pmax(0, center - margin)
          ci_upper_prop <- pmin(1, center + margin)

          list(ci_lower_prop * 100, ci_upper_prop * 100)
        }]
      } else {
        # Normal approximation (Wald interval)
        results[, c("ci_lower", "ci_upper") := {
          n <- denominator
          p <- proportion
          se <- sqrt(p * (1 - p) / n)
          margin <- qnorm(0.975) * se

          ci_lower_prop <- pmax(0, p - margin)
          ci_upper_prop <- pmin(1, p + margin)

          list(ci_lower_prop * 100, ci_upper_prop * 100)
        }]
      }
    } else {
      # WEIGHTED CONFIDENCE INTERVALS
      # Using design-adjusted standard error for weighted proportions
      results[, c("ci_lower", "ci_upper") := {
        p <- proportion
        w_sum <- denominator
        w2_sum <- sum_weights_squared

        # Design effect adjusted standard error
        se_weighted <- sqrt(p * (1 - p) * w2_sum / (w_sum^2))
        margin <- qnorm(0.975) * se_weighted

        ci_lower_prop <- pmax(0, p - margin)
        ci_upper_prop <- pmin(1, p + margin)

        list(ci_lower_prop * 100, ci_upper_prop * 100)
      }]
    }

    # Round CI bounds
    results[, ci_lower := round(ci_lower, round_digits)]
    results[, ci_upper := round(ci_upper, round_digits)]
  }

  # Round percentage
  results[, percentage := round(percentage, round_digits)]

  # Remove rows where any grouping variable is NA (only if group_vars is not NULL)
  if (!is.null(group_vars)) {
    for (var in group_vars) {
      results <- results[!is.na(get(var))]
    }
  }

  # Clean up and reorder columns
  if (include_diagnostics) {
    if (is.null(weight_var)) {
      # Unweighted diagnostics
      base_cols <- c("percentage", "calculation_type", "n_total",
                    "n_valid", "n_outcome_1", "n_outcome_0", "n_outcome_na")
    } else {
      # Weighted diagnostics
      base_cols <- c("percentage", "calculation_type", "n_total",
                    "n_valid", "n_outcome_1", "n_outcome_0", "n_outcome_na", "sum_weights")
    }
  } else {
    base_cols <- c("percentage", "calculation_type")
  }

  # Add CI columns if requested
  if (include_ci) {
    base_cols <- c(base_cols, "ci_lower", "ci_upper")
  }

  # Combine group_vars (if any) with base columns
  if (is.null(group_vars)) {
    final_cols <- base_cols
  } else {
    final_cols <- c(group_vars, base_cols)
  }

  # Remove intermediate columns
  cols_to_remove <- c("numerator", "denominator", "proportion")
  if (!is.null(weight_var)) {
    cols_to_remove <- c(cols_to_remove, "sum_weights_squared")
  }

  existing_cols_to_remove <- intersect(cols_to_remove, names(results))
  if (length(existing_cols_to_remove) > 0) {
    results[, (existing_cols_to_remove) := NULL]
  }

  results <- results[, ..final_cols]

  return(results)
}

#' Calculate Percentage with Automatic Method Detection
#'
#' Wrapper function that automatically detects if weight variable exists
#' and calls calc_percentage_ci accordingly.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the binary outcome variable
#' @param group_vars Character vector. Names of grouping variables
#' @param weight_var Character string. Name of the weight variable (if it exists in dt)
#' @param ... Additional arguments passed to calc_percentage_ci
#'
#' @return A data.table with percentages, confidence intervals, and calculation type
#'
#' @examples
#' # Will use weighted calculation if VEKT exists, unweighted otherwise
#' calc_percentage_ci_auto(dt, "ltp_cannabis", "agecat", "VEKT", denominator_var = "canpop")
#'
#' @export
calc_percentage_ci_auto <- function(dt, outcome_var, group_vars, weight_var = NULL, ...) {

  # Check if weight variable exists and has valid values
  use_weights <- FALSE
  if (!is.null(weight_var)) {
    if (weight_var %in% names(dt)) {
      weight_values <- dt[[weight_var]]
      if (is.numeric(weight_values) && !all(is.na(weight_values))) {
        use_weights <- TRUE
      }
    }
  }

  if (use_weights) {
    message(paste("Using weighted calculation with variable:", weight_var))
    return(calc_percentage_ci(dt, outcome_var, group_vars, weight_var = weight_var, ...))
  } else {
    if (!is.null(weight_var)) {
      message(paste("Weight variable", weight_var, "not found or invalid. Using unweighted calculation."))
    } else {
      message("No weight variable specified. Using unweighted calculation.")
    }
    return(calc_percentage_ci(dt, outcome_var, group_vars, weight_var = NULL, ...))
  }
}

# Example usage:
#
# # Overall weighted percentage with CI (no grouping variables)
# result1 <- calc_percentage_ci(dt, "ltp_cannabis", weight_var = "VEKT",
#                           denominator_var = "canpop")
#
# # Overall unweighted percentage with CI (no grouping variables)
# result2 <- calc_percentage_ci(dt, "ltp_cannabis", denominator_var = "canpop")
#
# # Weighted percentage with CI (when VEKT is provided and exists)
# result3 <- calc_percentage_ci(dt, "ltp_cannabis", "agecat", weight_var = "VEKT",
#                           denominator_var = "canpop")
#
# # Unweighted percentage with CI (when weight_var is NULL)
# result4 <- calc_percentage_ci(dt, "ltp_cannabis", "agecat", denominator_var = "canpop")
#
# # Multiple grouping variables - weighted with CI
# result5 <- calc_percentage_ci(dt, "ltp_cannabis", c("agecat", "gender"),
#                           weight_var = "VEKT", denominator_var = "canpop")
#
# # Without confidence intervals
# result6 <- calc_percentage_ci(dt, "ltp_cannabis", "agecat",
#                           weight_var = "VEKT", denominator_var = "canpop",
#                           include_ci = FALSE)
#
# # Using normal approximation for unweighted CI
# result7 <- calc_percentage_ci(dt, "ltp_cannabis", "agecat",
#                           denominator_var = "canpop", ci_method = "normal")
#
# print(result1)  # Overall percentage with CI
# print(result3)  # By age group with CI
