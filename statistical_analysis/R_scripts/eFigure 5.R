library(tidyverse)
library(gt)
library(gtsummary)
library(forcats)
library(glue)
library(forestmodel)
library(labelled)
library(jsonlite)
library(patchwork)

################################################################################

# Get hostname
hostname <- tolower(Sys.info()[["nodename"]])

# Determine script directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getSourceEditorContext()$path
} else {
  stop("Script path could not be determined. Please set the path manually.")
}
script_dir <- dirname(normalizePath(script_path))

# Construct path to config file (two directories up)
config_path <- file.path(script_dir, "..", "..", "path.config")
config_path <- normalizePath(config_path)
# Read config
config <- fromJSON(config_path)
# Access data and output root for current hostname
data_root <- config[[hostname]][["data_root"]]
output_root <- config[[hostname]][["output_root"]]
R_output_root <- config[[hostname]][["R_output_root"]]

#################################################################################
file_prefix <- file.path(output_root, "Final")

UF_and_FB <- read.csv(glue("{file_prefix}/Fluid_and_Ultrafiltration_Total.csv")) %>% 
  dplyr::select(patid, 
                mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                UF_AUC, unlimited_UF_increase_24h,
                mean_dm_balancerate_h, mean_dm_balancerate_h_48h, median_dm_balancerate_h,
                mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, Q3_UFnet_bin,
                mean_FB_bin, mean_FB_bin_48h, mean_FB_bin_FBneg, Q1_FB_bin)
mortality <- read.csv(glue("{file_prefix}/stays_fused_Total.csv")) 
combined <- inner_join(UF_and_FB, mortality, by="patid")

################################################################################

# Bin levels
lower_UFbin_thresh <- 1.01
upper_UFbin_thresh <- 1.75
neutral_bin_limit <- 20.833
negative_bin_limit <- 62.5

################################################################################
## Prepare data for low as reference group

combined <- inner_join(UF_and_FB, mortality, by="patid")
combined$mean_UFnet_bin <- factor(combined$mean_UFnet_bin, 
                                  levels = c(glue("low (<{lower_UFbin_thresh})"),
                                             glue("high (>{upper_UFbin_thresh})"),
                                             glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                             "zero"),
                                  labels = c("low (0.01-1.0)","high (>1.75)","moderate (1.01-1.75)","zero (0)"))
combined$Q3_UFnet_bin <- factor(combined$Q3_UFnet_bin, 
                                levels = c(glue("low (<{lower_UFbin_thresh})"),
                                           glue("high (>{upper_UFbin_thresh})"),
                                           glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                           "zero"),
                                labels = c("low (0.01-1.0)","high (>1.75)","moderate (1.01-1.75)","zero (0)"))
combined$fluidbalancechangeindL <- combined$mean_dm_balancerate_h / 10
combined$gender <- factor(combined$gender, levels = c("F", "M"), labels = c("Female", "Male"))
combined <- combined %>%
  set_variable_labels(
    mean_UFnet_bin = "Mean net Ultrafiltration (ml/kg/h)",
    Q3_UFnet_bin = "75th percentile net Ultrafiltration (ml/kg/h)",
    mean_FB_bin = "Mean Fluid Balance Change (ml/h)",
    fluidbalancechangeindL = "Mean Fluid Balance Change (dl/h)",
    age_at_admission = "Age (years)",
    gender = "Sex",
    BMI = "Body Mass Index (kg/m^2)", 
    emergency_admission = "Emergency Admission",
    apache_score = "APACHE score at admission",
    SOFA_score = "SOFA score at start of CRRT",
    invasive_ventilation = "Invasive Ventilation at start of CRRT")

###############################################################################

##Prepare for Mediation Analysis:

## For Mean_UFnet_bin and Low as reference Group:
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_mean_low <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("Step 1: Association between mean UFnet group and 28-day mortality", subtitle = "Reference: Low NUF rate")
# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE) + ggtitle("Step 2: Association between fluid balance change and 28-day mortality")
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_mean_low <- forest_model(step3_model) + ggtitle("Step 3: Association between mean UFnet group and fluid balance change", subtitle = "Reference: Low NUF rate") 

## For Q3_UFnet_bin and low as reference group!
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_Q3_low <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("Step 1: Association between Q3 UFnet group and 28-day mortality", subtitle = "Reference: Low NUF rate")
# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE) + ggtitle("Step 2: Association between fluid balance change and 28-day mortality")
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_Q3_low <- forest_model(step3_model) + ggtitle("Step 3: Association between upper quartile UFnet group and mean fluid balance change", subtitle = "Reference: Low NUF rate")

################################################################################
################################################################################

## Prepare data for moderate as reference group
combined <- inner_join(UF_and_FB, mortality, by="patid")

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

combined$gender <- factor(combined$gender, levels = c("F", "M"), labels = c("Female", "Male"))
combined$fluidbalancechangeindL <- combined$mean_dm_balancerate_h / 10

combined <- combined %>%
  set_variable_labels(
    mean_UFnet_bin = "Mean net Ultrafiltration (ml/kg/h)",
    Q3_UFnet_bin = "75th percentile net Ultrafiltration (ml/kg/h)",
    mean_FB_bin = "Mean Fluid Balance Change (ml/h)",
    fluidbalancechangeindL = "Mean Fluid Balance Change (dL/h)",
    age_at_admission = "Age (years)",
    gender = "Sex",
    BMI = "Body Mass Index (kg/m^2)",
    emergency_admission = "Emergency Admission",
    apache_score = "APACHE score at admission",
    SOFA_score = "SOFA score at start of CRRT",
    invasive_ventilation = "Invasive Ventilation at start of CRRT")

################################################################################
################################################################################

### Preparation for Mediation Analysis: 

## For Mean_UFnet_bin and moderate as reference group!
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_mean_moderate <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("", "Reference: Moderate NUF rate")
# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE)
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_mean_moderate <- forest_model(step3_model) + ggtitle("", "Reference: Moderate NUF rate")

## For Q3_UFnet_bin and moderate as reference group!
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_Q3_moderate <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("", "Reference: Moderate NUF rate")
# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE) + ggtitle("Step 2: Association between fluid balance change and 28-day mortality")
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_Q3_moderate <- forest_model(step3_model) + ggtitle("", "Reference: Moderate NUF rate")

################################################################################
################################################################################

## Prepare data for high as reference group

combined <- inner_join(UF_and_FB, mortality, by="patid")
combined$mean_UFnet_bin <- factor(combined$mean_UFnet_bin, 
                                  levels = c(glue("high (>{upper_UFbin_thresh})"),
                                             glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                             glue("low (<{lower_UFbin_thresh})"),
                                             "zero"),
                                  labels = c("high (>1.75)","moderate (1.01-1.75)","low (0.01-1.0)","zero (0)"))
combined$Q3_UFnet_bin <- factor(combined$Q3_UFnet_bin, 
                                levels = c(glue("high (>{upper_UFbin_thresh})"),
                                           glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                           glue("low (<{lower_UFbin_thresh})"),
                                           "zero"),
                                labels = c("high (>1.75)","moderate (1.01-1.75)","low (0.01-1.0)","zero (0)"))
combined$fluidbalancechangeindL <- combined$mean_dm_balancerate_h / 10
combined$gender <- factor(combined$gender, levels = c("F", "M"), labels = c("Female", "Male"))
combined <- combined %>%
  set_variable_labels(
    mean_UFnet_bin = "Mean net Ultrafiltration (ml/kg/h)",
    Q3_UFnet_bin = "75th percentile net Ultrafiltration (ml/kg/h)",
    mean_FB_bin = "Mean Fluid Balance Change (ml/h)",
    fluidbalancechangeindL = "Mean Fluid Balance Change (dl/h)",
    age_at_admission = "Age (years)",
    gender = "Sex",
    BMI = "Body Mass Index (kg/m^2)", 
    emergency_admission = "Emergency Admission",
    apache_score = "APACHE score at admission",
    SOFA_score = "SOFA score at start of CRRT",
    invasive_ventilation = "Invasive Ventilation at start of CRRT")

################################################################################

##Prepare for Mediation Analysis:

## For Mean_UFnet_bin and High as reference Group:
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_mean_high <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("", "Reference: High NUF rate")

# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE) + ggtitle("Step 2: Association between fluid balance change and 28-day mortality")
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ mean_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_mean_high <- forest_model(step3_model) + ggtitle("", "Reference: High NUF rate")

## For Q3_UFnet_bin and High as reference group!
# Step 1: log regression model of UF and Mortality
step1_model <- glm(outcome_death_28d ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step1_Q3_high <- forest_model(step1_model, exponentiate = TRUE) + ggtitle("", "Reference: High NUF rate")
# Step 2: log regression model of FB and Mortality
step2_model <- glm(outcome_death_28d ~ fluidbalancechangeindL + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined, family=binomial)
step2 <- forest_model(step2_model, exponentiate = TRUE) + ggtitle("Step 2: Association between fluid balance change and 28-day mortality")
# Step 3: linear regression of FB and UF 
step3_model <- glm(mean_dm_balancerate_h ~ Q3_UFnet_bin + age_at_admission + gender + BMI + emergency_admission + apache_score + SOFA_score + invasive_ventilation, data = combined)
step3_Q3_high <- forest_model(step3_model) + ggtitle("", "Reference: High NUF rate")

###############################################################################
# Save each Step
###############################################################################

#Step 1 for all reference groups (Low, Moderate, High) once for mean_UFnet_bin (5a), once for Q3_UFnet_bin (5b)
ggsave(plot= step1_mean_low / step1_mean_moderate / step1_mean_high, filename = glue("{R_output_root}/eFigure 5a.png"),
       width = 11, height = 15)
ggsave(plot= step1_Q3_low / step1_Q3_moderate / step1_Q3_high, filename = glue("{R_output_root}/eFigure 5b.png"),
       width = 11, height = 15)

###############################################################################

#Step 2 
ggsave(plot= step2, filename = glue("{R_output_root}/eFigure 5c.png"),
       width = 10, height = 5)

###############################################################################

# Step 3 for all reference groups (Low, Moderate, High) once for mean_UFnet_bin (5d), once for Q3_UFnet_bin (5e)
ggsave(plot= step3_mean_low / step3_mean_moderate / step3_mean_high, filename = glue("{R_output_root}/eFigure 5d.png"),
       width = 11, height = 15)
ggsave(plot= step3_Q3_low / step3_Q3_moderate / step3_Q3_high, filename = glue("{R_output_root}/eFigure 5e.png"),
       width = 11, height = 15)

###############################################################################
# Save eFigure5 as in the paper
###############################################################################

combined_plot <- step2 / step3_mean_high + 
  plot_layout(heights = c(1,1)) +
  plot_annotation(tag_levels = 'a')

# Save the combined figure 5
ggsave(plot = combined_plot, filename = glue("{R_output_root}/eFigure5.png"), width = 16, height = 30)