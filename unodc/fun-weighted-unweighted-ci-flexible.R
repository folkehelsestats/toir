library(data.table)

#' Calculate Weighted or Unweighted Percentages for Binary or Categorical Outcomes
#'
#' This function calculates percentages for binary (0/1) or categorical outcomes,
#' either overall or by grouping variables. Supports weighted and unweighted calculations
#' with 95% confidence intervals.
#'
#' @param dt A data.table containing the data
#' @param outcome_var Character string. Name of the outcome variable
#' @param outcome_type Character. Type of outcome: "binary" (default) or "categorical"
#' @param outcome_levels Vector. For categorical outcomes, specify which levels to calculate percentages for.
#'        If NULL (default), calculates for all unique values. For binary, defaults to calculating for value 1.
#' @param group_vars Character vector. Names of grouping variables (optional)
#' @param weight_var Character string. Name of the weight variable (optional)
#' @param denominator_var Character string. Name of variable defining the denominator population (optional)
#' @param denominator_value Numeric. Value of denominator_var to include in calculations (default: 1)
#' @param na_treatment Character. How to treat NA values: "exclude" (default) or "as_zero"
#' @param round_digits Integer. Number of decimal places for percentage (default: 2)
#' @param include_diagnostics Logical. Whether to include diagnostic columns (default: TRUE)
#' @param include_ci Logical. Whether to include 95% confidence intervals (default: TRUE)
#' @param ci_method Character. Method for CI: "wilson" (default for unweighted), "normal" (for weighted)
#' @param return_format Character. "long" (default) returns one row per category, "wide" returns categories as columns
#'
#' @return A data.table with grouping variables, percentages, confidence intervals, and optional diagnostics
#'
#' @examples
#' # Binary outcome (same as before)
#' calc_percentage_flexible(dt, "ltp_cannabis", outcome_type = "binary",
#'                         weight_var = "VEKT", denominator_var = "canpop")
#'
#' # Categorical outcome - all categories
#' calc_percentage_flexible(dt, "education_level", outcome_type = "categorical",
#'                         group_vars = "agecat", weight_var = "VEKT")
#'
#' # Categorical outcome - specific categories only
#' calc_percentage_flexible(dt, "education_level", outcome_type = "categorical",
#'                         outcome_levels = c("Bachelor", "Master", "PhD"),
#'                         group_vars = "agecat")
#'
#' @export
calc_percentage_flexible <- function(dt,
                                    outcome_var,
                                    outcome_type = "binary",
                                    outcome_levels = NULL,
                                    group_vars = NULL,
                                    weight_var = NULL,
                                    denominator_var = NULL,
                                    denominator_value = 1,
                                    na_treatment = "exclude",
                                    round_digits = 2,
                                    include_diagnostics = TRUE,
                                    include_ci = TRUE,
                                    ci_method = "wilson",
                                    return_format = "long") {

  # Input validation
  if (!is.data.table(dt)) {
    stop("Input 'dt' must be a data.table")
  }

  if (!outcome_type %in% c("binary", "categorical")) {
    stop("outcome_type must be 'binary' or 'categorical'")
  }

  if (!return_format %in% c("long", "wide")) {
    stop("return_format must be 'long' or 'wide'")
  }

  required_cols <- outcome_var
  if (!is.null(group_vars)) required_cols <- c(required_cols, group_vars)
  if (!is.null(weight_var)) required_cols <- c(required_cols, weight_var)
  if (!is.null(denominator_var)) required_cols <- c(required_cols, denominator_var)

  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
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

  # Determine outcome levels
  if (outcome_type == "binary") {
    if (is.null(outcome_levels)) {
      outcome_levels <- 1  # Default for binary
    }
  } else {
    # Categorical
    if (is.null(outcome_levels)) {
      # Get all unique non-NA values
      outcome_levels <- sort(unique(dt_work[[outcome_var]][!is.na(dt_work[[outcome_var]])]))
    }
  }

  # Initialize results list
  all_results <- list()

  # Calculate for each outcome level
  for (level in outcome_levels) {

    # Create indicator for this level
    if (na_treatment == "exclude") {
      dt_work[, outcome_indicator := ifelse(is.na(get(outcome_var)), NA,
                                           ifelse(get(outcome_var) == level, 1, 0))]
    } else {
      dt_work[, outcome_indicator := ifelse(get(outcome_var) == level, 1, 0)]
      dt_work[is.na(outcome_indicator), outcome_indicator := 0]
    }

    # Calculate based on weighted/unweighted and grouped/overall
    if (is.null(weight_var)) {
      # UNWEIGHTED
      if (is.null(group_vars)) {
        results <- dt_work[, .(
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          n_total = .N,
          n_level = sum(get(outcome_var) == level, na.rm = TRUE),
          calculation_type = "unweighted"
        )]
      } else {
        results <- dt_work[, .(
          numerator = sum(outcome_indicator, na.rm = (na_treatment == "exclude")),
          denominator = if (na_treatment == "exclude") sum(!is.na(outcome_indicator)) else .N,
          n_total = .N,
          n_level = sum(get(outcome_var) == level, na.rm = TRUE),
          calculation_type = "unweighted"
        ), by = group_vars]
      }
    } else {
      # WEIGHTED
      if (is.null(group_vars)) {
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
          n_level = sum(get(outcome_var) == level, na.rm = TRUE),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        )]
      } else {
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
          n_level = sum(get(outcome_var) == level, na.rm = TRUE),
          sum_weights = sum(get(weight_var)),
          calculation_type = "weighted"
        ), by = group_vars]
      }
    }

    # Calculate percentage and proportion
    results[, percentage := (numerator / denominator) * 100]
    results[, proportion := numerator / denominator]

    # Calculate confidence intervals
    if (include_ci) {
      if (is.null(weight_var)) {
        # UNWEIGHTED CI
        if (ci_method == "wilson") {
          results[, c("ci_lower", "ci_upper") := {
            n <- denominator
            p <- proportion
            z <- qnorm(0.975)

            center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
            margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)

            list(pmax(0, center - margin) * 100, pmin(1, center + margin) * 100)
          }]
        } else {
          results[, c("ci_lower", "ci_upper") := {
            n <- denominator
            p <- proportion
            se <- sqrt(p * (1 - p) / n)
            margin <- qnorm(0.975) * se

            list(pmax(0, p - margin) * 100, pmin(1, p + margin) * 100)
          }]
        }
      } else {
        # WEIGHTED CI
        results[, c("ci_lower", "ci_upper") := {
          p <- proportion
          se_weighted <- sqrt(p * (1 - p) * sum_weights_squared / (denominator^2))
          margin <- qnorm(0.975) * se_weighted

          list(pmax(0, p - margin) * 100, pmin(1, p + margin) * 100)
        }]
      }

      results[, `:=`(ci_lower = round(ci_lower, round_digits),
                     ci_upper = round(ci_upper, round_digits))]
    }

    # Round percentage
    results[, percentage := round(percentage, round_digits)]

    # Add outcome level identifier
    results[, outcome_level := level]

    # Store in list
    all_results[[as.character(level)]] <- results
  }

  # Combine all results
  final_results <- rbindlist(all_results, fill = TRUE)

  # Remove rows with NA in grouping variables
  if (!is.null(group_vars)) {
    for (var in group_vars) {
      final_results <- final_results[!is.na(get(var))]
    }
  }

  # Select and order columns
  if (include_diagnostics) {
    if (is.null(weight_var)) {
      keep_cols <- c("outcome_level", "percentage", "calculation_type",
                    "n_total", "n_level")
    } else {
      keep_cols <- c("outcome_level", "percentage", "calculation_type",
                    "n_total", "n_level", "sum_weights")
    }
  } else {
    keep_cols <- c("outcome_level", "percentage", "calculation_type")
  }

  if (include_ci) {
    keep_cols <- c(keep_cols, "ci_lower", "ci_upper")
  }

  if (!is.null(group_vars)) {
    keep_cols <- c(group_vars, keep_cols)
  }

  # Remove intermediate columns
  cols_to_remove <- c("numerator", "denominator", "proportion", "sum_weights_squared", "outcome_indicator")
  final_results[, (intersect(cols_to_remove, names(final_results))) := NULL]

  # Ensure we only keep columns that exist
  keep_cols <- intersect(keep_cols, names(final_results))
  final_results <- final_results[, ..keep_cols]

  # Convert to wide format if requested
  if (return_format == "wide" && outcome_type == "categorical") {
    id_vars <- if (!is.null(group_vars)) group_vars else "calculation_type"

    value_vars <- c("percentage")
    if (include_ci) value_vars <- c(value_vars, "ci_lower", "ci_upper")

    final_results <- dcast(final_results,
                          as.formula(paste(paste(id_vars, collapse = " + "), "~ outcome_level")),
                          value.var = value_vars)
  }

  return(final_results)
}


#' Backward Compatible Binary Function
#'
#' Wrapper that maintains the original function signature for binary outcomes
#'
#' @export
calc_percentage_ci <- function(dt, outcome_var, group_vars = NULL, weight_var = NULL, ...) {
  calc_percentage_flexible(
    dt = dt,
    outcome_var = outcome_var,
    outcome_type = "binary",
    outcome_levels = 1,
    group_vars = group_vars,
    weight_var = weight_var,
    ...
  )
}


# Example usage:
#
# # Binary outcome (backward compatible)
# result1 <- calc_percentage_ci(dt, "ltp_cannabis", "agecat",
#                              weight_var = "VEKT", denominator_var = "canpop")
#
# # Categorical outcome - long format (default)
# result2 <- calc_percentage_flexible(dt, "education_level",
#                                    outcome_type = "categorical",
#                                    group_vars = "agecat",
#                                    weight_var = "VEKT")
#
# # Categorical outcome - wide format
# result3 <- calc_percentage_flexible(dt, "education_level",
#                                    outcome_type = "categorical",
#                                    group_vars = "agecat",
#                                    return_format = "wide")
#
# # Categorical - specific levels only
# result4 <- calc_percentage_flexible(dt, "drug_use",
#                                    outcome_type = "categorical",
#                                    outcome_levels = c("cannabis", "cocaine"),
#                                    group_vars = c("agecat", "gender"),
#                                    weight_var = "VEKT")
#
# # Overall categorical (no grouping)
# result5 <- calc_percentage_flexible(dt, "education_level",
#                                    outcome_type = "categorical",
#                                    weight_var = "VEKT")
