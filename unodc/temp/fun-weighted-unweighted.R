library(data.table)

#' Calculate Weighted or Unweighted Percentages by Group or Overall
#'
#' This function calculates percentages of a binary outcome variable either overall
#' or by one or more grouping variables. It can perform both weighted and unweighted
#' calculations depending on whether a weight variable is provided.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the binary outcome variable (should be 0/1 or logical)
#' @param group_vars Character vector. Names of grouping variables (optional). If NULL, calculates overall percentage (optional). If NULL, calculates overall percentage
#' @param weight_var Character string. Name of the weight variable (optional). If NULL, unweighted percentages are calculated
#' @param denominator_var Character string. Name of variable defining the denominator population (optional)
#' @param denominator_value Numeric. Value of denominator_var to include in calculations (default: 1)
#' @param na_treatment Character. How to treat NA values in outcome_var: "exclude" (default) or "as_zero"
#' @param round_digits Integer. Number of decimal places for percentage (default: 2)
#' @param include_diagnostics Logical. Whether to include diagnostic columns (default: TRUE)
#'
#' @return A data.table with grouping variables, percentages, and optional diagnostic information
#'
#' @details
#' ## Weighted Percentage (when weight_var is provided):
#' Weighted Percentage = (Σ(weight × I(outcome = 1))) / (Σ(weight)) × 100
#'
#' ## Unweighted Percentage (when weight_var is NULL):
#' Unweighted Percentage = (Count(outcome = 1)) / (Count(total)) × 100
#'
#' Where I(outcome = 1) is an indicator function that equals 1 when outcome = 1, 0 otherwise.
#'
#' When a denominator variable is specified, only rows where denominator_var == denominator_value
#' are included in the calculation.
#'
#' @examples
#' # Overall weighted percentage (no grouping)
#' calc_percentage(dt, "ltp_cannabis", weight_var = "VEKT", denominator_var = "canpop")
#'
#' # Overall unweighted percentage (no grouping)
#' calc_percentage(dt, "ltp_cannabis", denominator_var = "canpop")
#'
#' # Weighted percentage with single grouping variable
#' calc_percentage(dt, "ltp_cannabis", "agecat", weight_var = "VEKT",
#'                denominator_var = "canpop")
#'
#' # Unweighted percentage with multiple grouping variables
#' calc_percentage(dt, "ltp_cannabis", c("agecat", "gender"))
#'
#' # Weighted percentage with multiple grouping variables
#' calc_percentage(dt, "ltp_cannabis", c("agecat", "gender"), weight_var = "VEKT",
#'                denominator_var = "canpop")
#'
#' @author Your Name
#' @export
calc_percentage <- function(dt,
                           outcome_var,
                           group_vars = NULL,
                           weight_var = NULL,
                           denominator_var = NULL,
                           denominator_value = 1,
                           na_treatment = "exclude",
                           round_digits = 2,
                           include_diagnostics = TRUE) {

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
          calculation_type = "weighted"
        ), by = group_vars]
      }
    }
  }

  # Calculate percentage
  results[, percentage := (numerator / denominator) * 100]

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

  # Combine group_vars (if any) with base columns
  if (is.null(group_vars)) {
    final_cols <- base_cols
  } else {
    final_cols <- c(group_vars, base_cols)
  }

  # Remove intermediate columns
  results[, c("numerator", "denominator") := NULL]
  results <- results[, ..final_cols]

  return(results)
}

#' Calculate Percentage with Automatic Method Detection
#'
#' Wrapper function that automatically detects if weight variable exists
#' and calls calc_percentage accordingly.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the binary outcome variable
#' @param group_vars Character vector. Names of grouping variables
#' @param weight_var Character string. Name of the weight variable (if it exists in dt)
#' @param ... Additional arguments passed to calc_percentage
#'
#' @return A data.table with percentages and calculation type
#'
#' @examples
#' # Will use weighted calculation if VEKT exists, unweighted otherwise
#' calc_percentage_auto(dt, "ltp_cannabis", "agecat", "VEKT", denominator_var = "canpop")
#'
#' @export
calc_percentage_auto <- function(dt, outcome_var, group_vars, weight_var = NULL, ...) {

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
    return(calc_percentage(dt, outcome_var, group_vars, weight_var = weight_var, ...))
  } else {
    if (!is.null(weight_var)) {
      message(paste("Weight variable", weight_var, "not found or invalid. Using unweighted calculation."))
    } else {
      message("No weight variable specified. Using unweighted calculation.")
    }
    return(calc_percentage(dt, outcome_var, group_vars, weight_var = NULL, ...))
  }
}

# Example usage:
#
# # Overall weighted percentage (no grouping variables)
# result1 <- calc_percentage(dt, "ltp_cannabis", weight_var = "VEKT",
#                           denominator_var = "canpop")
#
# # Overall unweighted percentage (no grouping variables)
# result2 <- calc_percentage(dt, "ltp_cannabis", denominator_var = "canpop")
#
# # Weighted percentage (when VEKT is provided and exists)
# result3 <- calc_percentage(dt, "ltp_cannabis", "agecat", weight_var = "VEKT",
#                           denominator_var = "canpop")
#
# # Unweighted percentage (when weight_var is NULL)
# result4 <- calc_percentage(dt, "ltp_cannabis", "agecat", denominator_var = "canpop")
#
# # Multiple grouping variables - weighted
# result5 <- calc_percentage(dt, "ltp_cannabis", c("agecat", "gender"),
#                           weight_var = "VEKT", denominator_var = "canpop")
#
# # Multiple grouping variables - unweighted
# result6 <- calc_percentage(dt, "ltp_cannabis", c("agecat", "gender"),
#                           denominator_var = "canpop")
#
# # Automatic detection
# result7 <- calc_percentage_auto(dt, "ltp_cannabis", "agecat", "VEKT",
#                                denominator_var = "canpop")
#
# print(result1)  # Overall percentage
# print(result3)  # By age group
