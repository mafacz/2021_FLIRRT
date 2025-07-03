library(tidyverse)
library(readr)
library(mediation)
library(flexplot)
library(nnet)
library(glue)
library(gtsummary)
library(tidyr)
library(stringr)
library(gt)
library(broom)


# Load data
UFnet_FBchange <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Fluid_and_Ultrafiltration_Total.csv") %>% 
  dplyr::select(patid, 
                mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                UF_AUC, unlimited_UF_increase_24h,
                mean_dm_balancerate_h, mean_dm_balancerate_h_48h, mean_dm_balancerate_h_FBneg, median_dm_balancerate_h, Q3_dm_balancerate_h,
                mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, Q3_UFnet_bin, mean_FB_bin, mean_FB_bin_48h, mean_FB_bin_FBneg, Q1_FB_bin)

covariates <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\stays_fused_Total.csv")

combined <- inner_join(UFnet_FBchange, covariates, by = "patid")

# Thresholds
lower_UFbin_thresh <- 1.01
upper_UFbin_thresh <- 1.75
neutral_bin_limit <- 20.833
negative_bin_limit <- 62.5

# Recode bins
combined$mean_UFnet_bin <- factor(combined$mean_UFnet_bin, 
                                  levels = c(glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                             glue("high (>{upper_UFbin_thresh})"),
                                             glue("low (<{lower_UFbin_thresh})"),
                                             "zero"),
                                  labels = c("moderate (1.01-1.75)", "high (>1.75)", "low (0.01-1.0)", "zero (0)"))
combined$Q3_UFnet_bin <- factor(combined$Q3_UFnet_bin, 
                                levels = c(glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                           glue("high (>{upper_UFbin_thresh})"),
                                           glue("low (<{lower_UFbin_thresh})"),
                                           "zero"),
                                labels = c("moderate (1.01-1.75)", "high (>1.75)", "low (0.01-1.0)", "zero (0)"))
combined$mean_FB_bin <- factor(combined$mean_FB_bin, 
                               levels = c(glue("neutral (-{neutral_bin_limit} - {neutral_bin_limit})"),
                                          glue("strong_negative (< -{negative_bin_limit})"),
                                          glue("light_negative (-{negative_bin_limit} - -{neutral_bin_limit})"),
                                          glue("positive (>{neutral_bin_limit})")),
                               labels = c("neutral (-20.83 - 20.83)",
                                          "strong negative (<-62.5)",
                                          "light negative (-62.5 - -20.84)",
                                          "positive (>20.83)"))
combined$gender <- factor(combined$gender, levels = c("F", "M"), labels = c("Female", "Male"))

################################################################################
# Mediation analysis functions: one for mean, one for Q3 of Ultrafiltration rate

run_mediation_analysis_mean <- function(data, treat_level, control_level, group_label) {
  df <- data %>%
    filter(mean_UFnet_bin %in% c(treat_level, control_level), !is.na(apache_score)) %>%
    mutate(mean_UFnet_bin = factor(mean_UFnet_bin, levels = c(treat_level, control_level)),
           gender = as.factor(gender),
           emergency_admission = as.factor(emergency_admission)) %>% 
    droplevels()
  
  if (nlevels(df$mean_UFnet_bin) < 2) {
    warning(glue("Skipping {group_label}: Only one group present"))
    return(NULL)
  }
  
  # Models
  med_model <- glm(mean_dm_balancerate_h ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = df)
  out_model <- glm(outcome_death_28d ~ mean_dm_balancerate_h + mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = df)

  # Mediation
  set.seed(123)
  mediation_result <- mediate(med_model, out_model,
                              treat = "mean_UFnet_bin",
                              mediator = "mean_dm_balancerate_h",
                              treat.value = treat_level,
                              control.value = control_level,
                              boot = TRUE, sims = 1000)
  
  list(group = group_label,
       mediator_model = summary(med_model),
       outcome_model = summary(out_model),
       mediation_summary = summary(mediation_result))
}

run_mediation_analysis_Q3 <- function(data, treat_level, control_level, group_label) {
  df <- data %>%
    filter(Q3_UFnet_bin %in% c(treat_level, control_level), !is.na(apache_score)) %>%
    mutate(Q3_UFnet_bin = factor(Q3_UFnet_bin, levels = c(treat_level, control_level)),
           gender = as.factor(gender),
           emergency_admission = as.factor(emergency_admission)) %>% 
    droplevels()
  
  if (nlevels(df$Q3_UFnet_bin) < 2) {
    warning(glue("Skipping {group_label}: Only one group present"))
    return(NULL)
  }
  
  # Models
  med_model <- glm(mean_dm_balancerate_h ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = df)
  out_model <- glm(outcome_death_28d ~ mean_dm_balancerate_h + Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = df)

  # Mediation
  set.seed(123)
  mediation_result <- mediate(med_model, out_model,
                              treat = "Q3_UFnet_bin",
                              mediator = "mean_dm_balancerate_h",
                              treat.value = treat_level,
                              control.value = control_level,
                              boot = TRUE, sims = 1000)
  
  list(group = group_label,
       mediator_model = summary(med_model),
       outcome_model = summary(out_model),
       mediation_summary = summary(mediation_result))
}


#################################

summarize_mediation_results <- function(results) {
  extract_exact <- function(res) {
    med <- res$mediation_summary
    tibble(
      Comparison = res$group,
      ACME = round(med$d0, 4),
      ACME_CI = sprintf("[%.4f, %.4f]", med$d0.ci[1], med$d0.ci[2]),
      ACME_p = med$d0.p,
      ADE = round(med$z0, 4),
      ADE_CI = sprintf("[%.4f, %.4f]", med$z0.ci[1], med$z0.ci[2]),
      ADE_p = med$z0.p,
      Total_Effect = round(med$tau.coef, 4),
      TE_CI = sprintf("[%.4f, %.4f]", med$tau.ci[1], med$tau.ci[2]),
      TE_p = med$tau.p,
      Prop_Mediated = round(med$n0, 4),
      PM_CI = sprintf("[%.4f, %.4f]", med$n0.ci[1], med$n0.ci[2]),
      PM_p = med$n0.p
    )
  }
  
  extract_exp <- function(res) {
    med <- res$mediation_summary
    tibble(
      Comparison = res$group,
      ACME = round(exp(med$d0), 4),
      ACME_CI = sprintf("[%.4f, %.4f]", exp(med$d0.ci[1]), exp(med$d0.ci[2])),
      ACME_p = med$d0.p,
      ADE = round(exp(med$z0), 4),
      ADE_CI = sprintf("[%.4f, %.4f]", exp(med$z0.ci[1]), exp(med$z0.ci[2])),
      ADE_p = med$z0.p,
      Total_Effect = round(exp(med$tau.coef), 4),
      TE_CI = sprintf("[%.4f, %.4f]", exp(med$tau.ci[1]), exp(med$tau.ci[2])),
      TE_p = med$tau.p,
      Prop_Mediated = round(med$n0, 4),
      PM_CI = sprintf("[%.4f, %.4f]", med$n0.ci[1], med$n0.ci[2]),
      PM_p = med$n0.p
    )
  }
  
  exact_table <- purrr::map_dfr(results, extract_exact)
  exp_table <- purrr::map_dfr(results, extract_exp)
  
  list(
    exact = exact_table %>%
      gt() %>%
      tab_header(title = "Mediation Analysis: Exact Estimates"),
    exp = exp_table %>%
      gt() %>%
      tab_header(title = "Mediation Analysis")
  )
}

################################################################################
################################################################################

#########
# Run mediation comparisons for mean
results_mean <- list(
  high_mod = run_mediation_analysis_mean(combined, "high (>1.75)", "moderate (1.01-1.75)", "High vs Moderate"),
  high_low = run_mediation_analysis_mean(combined, "high (>1.75)", "low (0.01-1.0)", "High vs Low"),
  high_zero = run_mediation_analysis_mean(combined, "high (>1.75)", "zero (0)", "High vs Zero"),
  mod_low = run_mediation_analysis_mean(combined, "moderate (1.01-1.75)", "low (0.01-1.0)", "Moderate vs Low"),
  mod_zero = run_mediation_analysis_mean(combined, "moderate (1.01-1.75)", "zero (0)", "Moderate vs Zero"),
  low_zero = run_mediation_analysis_mean(combined, "low (0.01-1.0)", "zero (0)", "Low vs Zero")
)
results_mean

# Create Mediation summary tables for mean
tables_mean <- summarize_mediation_results(results_mean)

mean_mediation_table_exact <- tables_mean$exact

mean_mediation_table_exponentiated <- tables_mean$exp
gtsave(mean_mediation_table_exponentiated, "eTable_3a.docx", path = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\")

##########
# Run mediation comparisons for q3
results_Q3 <- list(
  high_mod = run_mediation_analysis_Q3(combined, "high (>1.75)", "moderate (1.01-1.75)", "High vs Moderate"),
  high_low = run_mediation_analysis_Q3(combined, "high (>1.75)", "low (0.01-1.0)", "High vs Low"),
  high_zero = run_mediation_analysis_Q3(combined, "high (>1.75)", "zero (0)", "High vs Zero"),
  mod_low = run_mediation_analysis_Q3(combined, "moderate (1.01-1.75)", "low (0.01-1.0)", "Moderate vs Low"),
  mod_zero = run_mediation_analysis_Q3(combined, "moderate (1.01-1.75)", "zero (0)", "Moderate vs Zero"),
  low_zero = run_mediation_analysis_Q3(combined, "low (0.01-1.0)", "zero (0)", "Low vs Zero")
)
results_Q3

# Create Mediation summary tables for q3
tables_Q3 <- summarize_mediation_results(results_Q3)

Q3_mediation_table_exact <- tables_Q3$exact

Q3_mediation_table_exponentiated <- tables_Q3$exp
gtsave(Q3_mediation_table_exponentiated, "eTable_3b.docx", path = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\")
