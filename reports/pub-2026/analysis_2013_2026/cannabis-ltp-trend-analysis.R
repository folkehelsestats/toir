
################################################################################
# Cannabis Use Trend Analysis - Simplified for Quarto
#
# Purpose: Analyze temporal trends in cannabis use with survey-weighted
#          logistic regression, generate highcharter plots for HTML output
#
# Outputs:
#   - plot_trend_ltp: Overall adjusted vs observed prevalence plot
#   - plot_by_gender_ltp: Gender-stratified trend plot
#   - trend_results_ltp: List containing key statistical results
#
# Usage in Quarto:
#   source("cannabis_trend_analysis.R")
#   print(plot_trend_ltp)
#   print(plot_by_gender_ltp)
################################################################################

# Load required packages
library(survey)      # Survey-weighted analyses
library(data.table)  # Data manipulation
library(highcharter) # Interactive plots

################################################################################
# 1. DATA PREPARATION
################################################################################

# Filter to eligible population and prepare variables
df_can <- DTT[ltpPop_cannabis == 1]  # Keep only eligible respondents

# Convert gender to factor with labels
df_can[, kjonn := factor(kjonn,
                         levels = c(1, 2),
                         labels = c("Male", "Female"))]

# Get unique years for predictions
years_unique <- sort(unique(df_can$year))

################################################################################
# 2. DEFINE SURVEY DESIGN
################################################################################

# Create survey design object with weights
# ids = ~1 indicates no clustering (simple random sampling)
design_can <- svydesign(
  ids = ~1,           # No clustering
  weights = ~vekt2,   # Survey weight variable
  data = df_can
)

################################################################################
# 3. FIT SURVEY-WEIGHTED MODELS
################################################################################

# --- OVERALL MODEL: Year as continuous (linear trend test) ---
model_trend <- svyglm(
  syv_ltpCan ~ year + alder + kjonn,
  design = design_can,
  family = quasibinomial()
)

# Extract trend statistics
year_coef <- coef(model_trend)["year"]
year_se <- sqrt(diag(vcov(model_trend)))["year"]
year_pval <- summary(model_trend)$coefficients["year", "Pr(>|t|)"]

# --- GENDER-STRATIFIED MODELS ---
design_male <- subset(design_can, kjonn == "Male")
design_female <- subset(design_can, kjonn == "Female")

# Model for males
model_male <- svyglm(
  syv_ltpCan ~ year + alder,
  design = design_male,
  family = quasibinomial()
)

# Model for females
model_female <- svyglm(
  syv_ltpCan ~ year + alder,
  design = design_female,
  family = quasibinomial()
)

# Test for interaction (different trends by gender?)
model_interaction <- svyglm(
  syv_ltpCan ~ year * kjonn + alder,
  design = design_can,
  family = quasibinomial()
)

interaction_test <- regTermTest(model_interaction, "year:kjonn")
interaction_pval <- interaction_test$p

# Extract gender-specific coefficients
male_coef <- coef(model_male)["year"]
male_pval <- summary(model_male)$coefficients["year", "Pr(>|t|)"]

female_coef <- coef(model_female)["year"]
female_pval <- summary(model_female)$coefficients["year", "Pr(>|t|)"]

################################################################################
# 4. CALCULATE PREDICTED PREVALENCES
################################################################################

# --- OVERALL ADJUSTED PREVALENCE ---
# Replicate dataset for each year (marginal standardization)
pred_data_all <- rbindlist(lapply(years_unique, function(yr) {
  df_can[, .(alder, kjonn, year = yr)]
}))

# Get predictions on log-odds scale
predictions_link <- predict(
  model_trend,
  newdata = pred_data_all,
  type = "link",
  se.fit = TRUE
)

# Convert to probability scale with confidence intervals
pred_data_all[, `:=`(
  pred_prob = as.vector(predictions_link),
  pred_se = sqrt(as.vector(attr(predictions_link, "var")))
)]

pred_data_all[, `:=`(
  pred_prevalence = plogis(pred_prob),
  lower_ci = plogis(pred_prob - 1.96 * pred_se),
  upper_ci = plogis(pred_prob + 1.96 * pred_se)
)]

# Aggregate by year (average across standardization sample)
pred_by_year <- pred_data_all[, .(
  predicted_prevalence = mean(pred_prevalence) * 100,
  lower_ci = mean(lower_ci) * 100,
  upper_ci = mean(upper_ci) * 100
), by = year]

setorder(pred_by_year, year)

# --- OBSERVED WEIGHTED PREVALENCE ---
obs_prev <- svyby(
  ~syv_ltpCan,
  ~year,
  design_can,
  svymean,
  na.rm = TRUE
)

obs_prev_dt <- as.data.table(obs_prev)
setnames(obs_prev_dt,
         old = c("syv_ltpCan", "se"),
         new = c("observed_prevalence", "obs_se"))

obs_prev_dt[, `:=`(
  observed_prevalence = observed_prevalence * 100,
  obs_lower_ci = (observed_prevalence - 1.96 * obs_se) * 100,
  obs_upper_ci = (observed_prevalence + 1.96 * obs_se) * 100,
  obs_se = obs_se * 100
)]

setorder(obs_prev_dt, year)

# --- GENDER-STRATIFIED PREDICTIONS ---
# Function to calculate predictions for gender-specific models
calc_gender_predictions <- function(model, design_subset, gender_label) {
  design_data <- design_subset$variables

  pred_data <- rbindlist(lapply(years_unique, function(yr) {
    data.table(alder = design_data$alder, year = yr)
  }))

  predictions_link <- predict(model, newdata = pred_data,
                              type = "link", se.fit = TRUE)

  pred_data[, `:=`(
    pred_prob = as.vector(predictions_link),
    pred_se = sqrt(as.vector(attr(predictions_link, "var")))
  )]

  pred_data[, `:=`(
    pred_prevalence = plogis(pred_prob),
    lower_ci = plogis(pred_prob - 1.96 * pred_se),
    upper_ci = plogis(pred_prob + 1.96 * pred_se)
  )]

  pred_by_year <- pred_data[, .(
    predicted_prevalence = mean(pred_prevalence) * 100,
    lower_ci = mean(lower_ci) * 100,
    upper_ci = mean(upper_ci) * 100,
    gender = gender_label
  ), by = year]

  setorder(pred_by_year, year)
  return(pred_by_year)
}

# Calculate for males and females
pred_male <- calc_gender_predictions(model_male, design_male, "Male")
pred_female <- calc_gender_predictions(model_female, design_female, "Female")
pred_by_gender <- rbind(pred_male, pred_female)

# Observed prevalence by gender
obs_by_gender <- svyby(
  ~syv_ltpCan,
  ~year + kjonn,
  design_can,
  svymean,
  na.rm = TRUE
)

obs_by_gender_dt <- as.data.table(obs_by_gender)
setnames(obs_by_gender_dt,
         old = c("kjonn", "syv_ltpCan", "se"),
         new = c("gender", "observed_prevalence", "obs_se"))

obs_by_gender_dt[, `:=`(
  observed_prevalence = observed_prevalence * 100,
  obs_lower_ci = (observed_prevalence - 1.96 * obs_se) * 100,
  obs_upper_ci = (observed_prevalence + 1.96 * obs_se) * 100,
  obs_se = obs_se * 100
)]

setorder(obs_by_gender_dt, gender, year)

################################################################################
# 5. CREATE HIGHCHARTER PLOTS
################################################################################

# --- PLOT 1: OVERALL ADJUSTED vs OBSERVED PREVALENCE ---
plot_trend_ltp <- highchart() %>%
  # Confidence interval for adjusted prevalence
  hc_add_series(
    data = as.data.frame(pred_by_year),
    type = "arearange",
    hcaes(x = year, low = lower_ci, high = upper_ci),
    name = "95% CI (Justert)",
    color = "steelblue",
    fillOpacity = 0.2,
    lineWidth = 0,
    marker = list(enabled = FALSE),
    enableMouseTracking = FALSE
  ) %>%
  # Adjusted prevalence line
  hc_add_series(
    data = as.data.frame(pred_by_year),
    type = "line",
    hcaes(x = year, y = predicted_prevalence),
    name = "Justert Prevalens",
    color = "steelblue",
    lineWidth = 3,
    marker = list(enabled = TRUE, radius = 5, fillColor = "steelblue", symbol = "circle")
  ) %>%
  # Confidence interval for observed prevalence
  hc_add_series(
    data = as.data.frame(obs_prev_dt),
    type = "arearange",
    hcaes(x = year, low = obs_lower_ci, high = obs_upper_ci),
    name = "95% CI (Observert)",
    color = "darkorange",
    fillOpacity = 0.15,
    lineWidth = 0,
    marker = list(enabled = FALSE),
    enableMouseTracking = FALSE
  ) %>%
  # Observed prevalence line
  hc_add_series(
    data = as.data.frame(obs_prev_dt),
    type = "line",
    hcaes(x = year, y = observed_prevalence),
    name = "Observert Prevalens",
    color = "darkorange",
    lineWidth = 3,
    dashStyle = "ShortDash",
    marker = list(enabled = TRUE, radius = 5, fillColor = "darkorange", symbol = "diamond")
  ) %>%
  hc_title(
    text = "Prevalens av cannabisbruk noen ganger: 2013 - 2025",
    style = list(fontWeight = "bold", fontSize = "16px")
  ) %>%
  hc_subtitle(
    text = "Justert estimat (kontrollert for alder og kjønn) og observert estimat (vektet prevalens)"
  ) %>%
  hc_xAxis(
    title = list(text = "År", style = list(fontWeight = "bold")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Prevalens av cannabisbruk (%)", style = list(fontWeight = "bold")),
    min = 0,
    max = 50
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    backgroundColor = "rgba(255, 255, 255, 0.95)",
    borderWidth = 1,
    borderColor = "#cccccc",
    valueDecimals = 2,
    valueSuffix = "%"
  ) %>%
  hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom") %>%
  hc_credits(enabled = TRUE, text = "Grå områder viser 95% konfidensintervaller") %>%
  hc_exporting(enabled = TRUE, filename = "cannabis_trend_adjusted_vs_observed") %>%
  hc_chart(backgroundColor = "#ffffff", style = list(fontFamily = "Arial, sans-serif"))

# --- PLOT 2: GENDER-STRATIFIED TRENDS ---
pred_male_df <- as.data.frame(pred_by_gender[gender == "Male"])
pred_female_df <- as.data.frame(pred_by_gender[gender == "Female"])
obs_male_df <- as.data.frame(obs_by_gender_dt[gender == "Male"])
obs_female_df <- as.data.frame(obs_by_gender_dt[gender == "Female"])

plot_by_gender_ltp <- highchart() %>%
  # MALES - Confidence interval
  hc_add_series(
    data = pred_male_df,
    type = "arearange",
    hcaes(x = year, low = lower_ci, high = upper_ci),
    name = "95% CI (Menn)",
    color = "#2E86AB",
    fillOpacity = 0.15,
    lineWidth = 0,
    marker = list(enabled = FALSE),
    enableMouseTracking = FALSE,
    linkedTo = "males_adj"
  ) %>%
  # MALES - Adjusted line
  hc_add_series(
    data = pred_male_df,
    type = "line",
    hcaes(x = year, y = predicted_prevalence),
    name = "Menn (Justert)",
    id = "males_adj",
    color = "#2E86AB",
    lineWidth = 3,
    marker = list(enabled = TRUE, radius = 5, fillColor = "#2E86AB", symbol = "circle")
  ) %>%
  # MALES - Observed line
  hc_add_series(
    data = obs_male_df,
    type = "line",
    hcaes(x = year, y = observed_prevalence),
    name = "Menn (Observert)",
    color = "#2E86AB",
    lineWidth = 2,
    dashStyle = "ShortDash",
    marker = list(enabled = TRUE, radius = 4, fillColor = "#2E86AB", symbol = "triangle")
  ) %>%
  # FEMALES - Confidence interval
  hc_add_series(
    data = pred_female_df,
    type = "arearange",
    hcaes(x = year, low = lower_ci, high = upper_ci),
    name = "95% CI (Kvinner)",
    color = "#A23B72",
    fillOpacity = 0.15,
    lineWidth = 0,
    marker = list(enabled = FALSE),
    enableMouseTracking = FALSE,
    linkedTo = "females_adj"
  ) %>%
  # FEMALES - Adjusted line
  hc_add_series(
    data = pred_female_df,
    type = "line",
    hcaes(x = year, y = predicted_prevalence),
    name = "Kvinner (Justert)",
    id = "females_adj",
    color = "#A23B72",
    lineWidth = 3,
    marker = list(enabled = TRUE, radius = 5, fillColor = "#A23B72", symbol = "circle")
  ) %>%
  # FEMALES - Observed line
  hc_add_series(
    data = obs_female_df,
    type = "line",
    hcaes(x = year, y = observed_prevalence),
    name = "Kvinner (Observert)",
    color = "#A23B72",
    lineWidth = 2,
    dashStyle = "ShortDash",
    marker = list(enabled = TRUE, radius = 4, fillColor = "#A23B72", symbol = "triangle-down")
  ) %>%
  hc_title(
    text = "Prevalens av cannabisbruk noen ganger etter kjønn over tid",
    style = list(fontWeight = "bold", fontSize = "16px")
  ) %>%
  hc_subtitle(
    text = "Kjønnsspesifikke trender: Heltrukne linjer = alderjustert; stiplede linjer = observert"
  ) %>%
  hc_xAxis(
    title = list(text = "År", style = list(fontWeight = "bold")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Prevalens av cannabisbruk (%)", style = list(fontWeight = "bold")),
    min = 0
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    backgroundColor = "rgba(255, 255, 255, 0.95)",
    borderWidth = 1,
    borderColor = "#cccccc",
    valueDecimals = 2,
    valueSuffix = "%"
  ) %>%
  hc_legend(enabled = TRUE, align = "right", verticalAlign = "top", layout = "vertical") %>%
  hc_credits(
    enabled = TRUE,
    text = paste0("Blå = Menn, Lilla = Kvinner; Skyggelagte områder = 95% KI")
  ) %>%
  hc_exporting(enabled = TRUE, filename = "cannabis_trend_by_gender") %>%
  hc_chart(backgroundColor = "#ffffff", style = list(fontFamily = "Arial, sans-serif"))

################################################################################
# 6. COMPILE RESULTS FOR REPORTING
################################################################################

# Store key results in a list for easy access in Quarto
trend_results_ltp <- list(
  # Overall trend
  overall = list(
    year_coefficient = year_coef,
    year_se = year_se,
    year_pvalue = year_pval,
    direction = ifelse(year_coef > 0, "økende", "synkende"),
    significant = year_pval < 0.05
  ),

  # Gender-stratified trends
  males = list(
    year_coefficient = male_coef,
    year_pvalue = male_pval,
    direction = ifelse(male_coef > 0, "økende", "synkende"),
    significant = male_pval < 0.05
  ),

  females = list(
    year_coefficient = female_coef,
    year_pvalue = female_pval,
    direction = ifelse(female_coef > 0, "økende", "synkende"),
    significant = female_pval < 0.05
  ),

  # Interaction test
  interaction = list(
    pvalue = interaction_pval,
    trends_differ = interaction_pval < 0.05
  ),

  # Data tables for custom reporting
  adjusted_prevalence = pred_by_year,
  observed_prevalence = obs_prev_dt,
  gender_adjusted = pred_by_gender,
  gender_observed = obs_by_gender_dt
)

################################################################################
# ANALYSIS COMPLETE
#
# Available objects:
#   - plot_trend_ltp: Overall prevalence plot
#   - plot_by_gender_ltp: Gender-stratified plot
#   - trend_results_ltp: List with all statistical results
#
# Example usage in Quarto:
#   ```{r}
#   source("cannabis_trend_analysis.R")
#   plot_trend_ltp
#   ```
#
#   Access results:
#   trend_results_ltp$overall$year_pvalue
#   trend_results_ltp$males$significant
################################################################################
