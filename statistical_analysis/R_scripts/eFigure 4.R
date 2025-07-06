library(tidyverse)
library(gt)
library(gtsummary)
library(forcats)
library(glue)
library(forestmodel)
library(labelled)
library(jsonlite)

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
combined$mean_UFnet_bin_48h <- factor(combined$mean_UFnet_bin_48h, 
                                      levels = c(glue("moderate ({lower_UFbin_thresh}-{upper_UFbin_thresh})"),
                                                 glue("high (>{upper_UFbin_thresh})"),
                                                 glue("low (<{lower_UFbin_thresh})"),
                                                 "zero"),
                                      labels = c("moderate (1.01-1.75)", "high (>1.75)", "low (0.01-1.0)", "zero (0)"))
combined$mean_UFnet_bin_UFpos <- factor(combined$mean_UFnet_bin_UFpos, 
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
combined$Q1_FB_bin <- factor(combined$Q1_FB_bin, 
                               levels = c(glue("neutral (-{neutral_bin_limit} - {neutral_bin_limit})"),
                                          glue("strong_negative (< -{negative_bin_limit})"),
                                          glue("light_negative (-{negative_bin_limit} - -{neutral_bin_limit})"),
                                          glue("positive (>{neutral_bin_limit})")),
                               labels = c("neutral (-20.83 - 20.83)",
                                          "strong negative (<-62.5)",
                                          "light negative (-62.5 - -20.84)",
                                          "positive (>20.83)"))
combined$mean_FB_bin_48h <- factor(combined$mean_FB_bin_48h, 
                                   levels = c(glue("neutral (-{neutral_bin_limit} - {neutral_bin_limit})"),
                                              glue("strong_negative (< -{negative_bin_limit})"),
                                              glue("light_negative (-{negative_bin_limit} - -{neutral_bin_limit})"),
                                              glue("positive (>{neutral_bin_limit})")),
                                   labels = c("neutral (-20.83 - 20.83)",
                                              "strong negative (<-62.5)",
                                              "light negative (-62.5 - -20.84)",
                                              "positive (>20.83)"))
combined$mean_FB_bin_FBneg <- factor(combined$mean_FB_bin_FBneg, 
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

combined <- combined %>%
  set_variable_labels(
    mean_UFnet_bin = "Mean net Ultrafiltration (ml/kg/h)",
    Q3_UFnet_bin = "75th percentile net Ultrafiltration (ml/kg/h)",
    mean_UFnet_bin_48h = "Mean net Ultrafiltration (ml/kg/h) during first 48 hours",
    mean_UFnet_bin_UFpos = "Mean net Ultrafiltration (ml/kg/h) when > 0",
    mean_FB_bin_FBneg =  "Mean Fluid Balance Change (ml/h) when < 0",
    mean_FB_bin = "Mean Fluid Balance Change (ml/h)",
    Q1_FB_bin = "75th percentile Fluid Balance Change (ml/h)",
    mean_FB_bin_48h = "Mean Fluid Balance Change (ml/h) during first 48 hours",
    UF_AUC = "Early Increase of net Ultrafiltration Rate (ml/kg)",
    age_at_admission = "Age (years)",
    gender = "Sex",
    emergency_admission = "Emergency Admission",
    apache_score = "APACHE score at admission",
    SOFA_score = "SOFA score at start of CRRT",
    invasive_ventilation = "Invasive Ventilation at start of CRRT")
labelled::var_label(combined$mean_UFnet_bin_UFpos)
labelled::var_label(combined$mean_FB_bin_FBneg)

model_both_mean <- glm(outcome_death_28d ~ mean_UFnet_bin + mean_FB_bin,
                           data = combined, family = binomial)
model_both_Q3<- glm(outcome_death_28d ~ Q3_UFnet_bin + mean_FB_bin,
                         data = combined, family = binomial)

eFigure_4a1 <- forest_model(model_both_mean, exponentiate = TRUE)
eFigure_4a2 <- forest_model(model_both_Q3, exponentiate = TRUE)

ggsave(plot= eFigure_4a1 / eFigure_4a2, filename =  glue("{R_output_root}/eFigure 4a.png"),
       width = 12, height = 8)

################################################################################

## Do the same log regression model for different summary statistics of UF and FB 

model_both_mean_48h_adj <- glm(outcome_death_28d ~ mean_UFnet_bin_48h + mean_FB_bin_48h + age_at_admission + gender + BMI +
                             emergency_admission + apache_score + SOFA_score + invasive_ventilation,
                           data = combined, family = binomial)
model_both_Q1_adj <- glm(outcome_death_28d ~ mean_UFnet_bin + Q1_FB_bin + age_at_admission + gender + BMI +
                           emergency_admission + apache_score + SOFA_score + invasive_ventilation,
                         data = combined, family = binomial)
model_both_UFpos_adj <- glm(outcome_death_28d ~ mean_UFnet_bin_UFpos + mean_FB_bin + age_at_admission + gender + BMI +
                              emergency_admission + apache_score + SOFA_score + invasive_ventilation,
                            data = combined, family = binomial)
model_both_FBneg_adj <- glm(outcome_death_28d ~ mean_UFnet_bin + mean_FB_bin_FBneg + age_at_admission + gender + BMI +
                           emergency_admission + apache_score + SOFA_score + invasive_ventilation,
                         data = combined, family = binomial)
model_both_AUC_adj <- glm(outcome_death_28d ~ UF_AUC + mean_FB_bin + age_at_admission + gender + BMI +
                            emergency_admission + apache_score + SOFA_score + invasive_ventilation,
                          data = combined, family = binomial)

eFigure_4b1 <- forest_model(model_both_mean_48h_adj, exponentiate = TRUE)
eFigure_4b1
ggsave(plot= eFigure_4b1, filename = glue("{R_output_root}/eFigure 4b1.png"),
       width = 12, height = 8)
eFigure_4b2 <- forest_model(model_both_Q1_adj, exponentiate = TRUE)
eFigure_4b2
ggsave(plot= eFigure_4b2, filename = glue("{R_output_root}/eFigure 4b2.png"),
       width = 12, height = 8)
eFigure_4b3 <- forest_model(model_both_UFpos_adj, exponentiate = TRUE)
eFigure_4b3
ggsave(plot= eFigure_4b3, filename = glue("{R_output_root}/eFigure 4b3.png"),
       width = 12, height = 8)
eFigure_4b4 <- forest_model(model_both_FBneg_adj, exponentiate = TRUE)
eFigure_4b4
ggsave(plot= eFigure_4b4, filename = glue("{R_output_root}/eFigure 4b4.png"),
       width = 12, height = 8)


eFigure_4b5 <- forest_model(model_both_AUC_adj, exponentiate = TRUE)
eFigure_4b5
ggsave(plot= eFigure_4b5, filename = glue("{R_output_root}/eFigure 4b5.png"),
       width = 12, height = 8)
