library(tidyverse)
library(gt)
library(gtsummary)
library(flextable)
library(officer)
library(glue)
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

## Load data

stay_info <- read.csv(glue("{file_prefix}/stays_fused_Total.csv"))
lab_values <- read.csv(glue("{file_prefix}/Lab_values_reg_Total.csv")) %>% 
  dplyr::select(patid, mean_vm2201_idx, mean_vm2105, max_vm2201_idx, max_vm2105,
                Noradrenalin_increase_0.1_mcgkgmin, Lactate_increase_2, Lactate_increase_4,
                Anuria_onset)
combined <- inner_join(stay_info, lab_values, by="patid")
################################################################################
################################################################################

data_table_outcomes <- combined
data_table_outcomes$Noradrenalin_increase_0.1_mcgkgmin <- factor(data_table_outcomes$Noradrenalin_increase_0.1_mcgkgmin, levels = c(2,1,0), labels = c("Elevated at start", "Increased during CRRT", "Never elevated"))
data_table_outcomes$Lactate_increase_2 <- factor(data_table_outcomes$Lactate_increase_2, levels = c(2,1,0), labels = c("Elevated at start", "Increased during CRRT", "Never elevated"))
data_table_outcomes$Lactate_increase_4 <- factor(data_table_outcomes$Lactate_increase_4, levels = c(2,1,0), labels = c("Elevated at start", "Increased during CRRT", "Never elevated"))
data_table_outcomes$Anuria_onset <- factor(data_table_outcomes$Anuria_onset, levels = c(2,1,0), labels = c("Anuria at start", "Anuria onset during CRRT", "Never anuric"))

labelestolabel_outcomes <- list(
  mean_vm2201_idx = "Mean Noradrenaline dose (mcg/kg/min)",
  max_vm2201_idx = "Maximum Noradrenaline dose (mcg/kg/min)",
  Noradrenalin_increase_0.1_mcgkgmin = "Noradrenaline > 0.1 mcg/kg/min",
  mean_vm2105 = "Mean Lactate level (mmol/l)",
  max_vm2105 = "Maximum Lactate level (mmol/l)",
  Lactate_increase_2 = "Lactate > 2 mmol/l",
  Lactate_increase_4 = "Lactate > 4 mmol/l",
  Anuria_onset = "Onset of Anuria (< 100ml/24h)"
)

Table_Outcomes <- data_table_outcomes %>% dplyr::select(source,
                                                        mean_vm2201_idx,max_vm2201_idx, 
                                                        mean_vm2105,max_vm2105,
                                                        Noradrenalin_increase_0.1_mcgkgmin,
                                                        Lactate_increase_2,
                                                        Lactate_increase_4,
                                                        Anuria_onset
) %>% 
  tbl_summary(
    by = source,
    label = labelestolabel_outcomes,
    missing = "no",
    statistic = list(mean_vm2201_idx ~ "{mean} ({sd})",
                     mean_vm2105 ~"{mean} ({sd})",
                     max_vm2201_idx ~ "{median} ({p25}-{p75})",
                     max_vm2105 ~ "{median} ({p25}-{p75})",
                     all_categorical() ~ "{n} ({p}%)"
    )) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Database**") %>%
  add_overall() 

Table_Outcomes

Table_Outcomes_gt <- as_gt(Table_Outcomes)
gtsave(Table_Outcomes_gt, filename = glue("{R_output_root}/eTable_2.docx"))
