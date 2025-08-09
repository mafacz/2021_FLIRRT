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

combined <- combined %>%
  set_variable_labels(
    mean_UFnet_bin = "Mean net Ultrafiltration (ml/kg/h)",
    Q3_UFnet_bin = "75th percentile net Ultrafiltration (ml/kg/h)",
    mean_FB_bin = "Mean Fluid Balance Change (ml/h)",
    age_at_admission = "Age (years)",
    gender = "Sex",
    BMI = "Body Mass Index (kg/m^2)",
    emergency_admission = "Emergency Admission",
    apache_score = "APACHE score at admission",
    SOFA_score = "SOFA score at start of CRRT",
    invasive_ventilation = "Invasive Ventilation at start of CRRT",
    session_length = "Duration of CRRT session")

model_both_mean_adj <- glm(outcome_death_28d ~ mean_UFnet_bin + mean_FB_bin + age_at_admission + gender + BMI +
                        emergency_admission + apache_score + SOFA_score + invasive_ventilation + session_length,
                      data = combined, family = binomial)
model_both_Q3_adj <- glm(outcome_death_28d ~ Q3_UFnet_bin + mean_FB_bin + age_at_admission + gender + BMI +
                        emergency_admission + apache_score + SOFA_score + invasive_ventilation + session_length,
                      data = combined, family = binomial)

Figure_3a <- forest_model(model_both_mean_adj, exponentiate = TRUE)
Figure_3b <- forest_model(model_both_Q3_adj, exponentiate = TRUE)

ggsave(plot= Figure_3a, filename = glue("{R_output_root}/Figure 3.png"),
       width = 17, height = 6)

###############################################################################
