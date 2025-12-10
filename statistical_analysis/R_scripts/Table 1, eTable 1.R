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

stay_info <- read.csv(glue("{file_prefix}/stays_fused_Total.csv"))
regular <- read.csv(glue("{file_prefix}/regular_UFperkg_Total.csv"))
lab_values <- read.csv(glue("{file_prefix}/Lab_values_reg_Total.csv"))
apache_groups <- read.csv(glue("{data_root}/apache_group.csv"))

first_non_na_or_nan <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) NaN else vals[1]
}

lab_values_at_start <- regular %>%
  filter(session_length < (6 * 60)) %>%
  group_by(patid) %>%
  summarise(vm2201_idx_first = first_non_na_or_nan(vm2201_idx),vm2105_first = first_non_na_or_nan(vm2105))

stay_info <- stay_info %>%  mutate(has_aki_or_acute_on_chronic = if_else(
      (has_aki == 1 | has_acute_on_chronic == 1) | (source == "HiRID" & has_eskd == 0),1,0))

data_tableone <- stay_info %>% dplyr::select(-session_length, -icu_stay_nr)
data_tableone <- left_join(data_tableone, lab_values_at_start, by="patid")

data_tableone$length_of_stay <- data_tableone$length_of_stay / (24*60*60) #change ICU LOS from seconds to days

data_tableone <- data_tableone %>% mutate(positive_fluidoverload = ifelse(fluidoverload >0, 1, 0),
                                          is_female = ifelse(gender == "F", 1, 0),
                                          icu_death = ifelse(outcome_icu_death == "True",1,0),
                                          death_28d = ifelse(outcome_death_28d == 1, 1,0))
#convert to labeled categories 
data_tableone$emergency_admission <- factor(data_tableone$emergency_admission, levels = c("True", "False"), labels = c("Yes", "No"))
data_tableone$adm_apache_group <- factor(data_tableone$adm_apache_group, levels = apache_groups$metavariable, labels = apache_groups$groupname_FLIRRT)
data_tableone$source <- factor(data_tableone$source, levels = c("AmsterdamUMCDb", "HiRID"), labels = c("Amsterdam UMC", "HiRID"))
data_tableone$fluidoverload_5 <- factor(data_tableone$fluidoverload_5, levels = c(1,0), labels = c("Yes", "No"))
data_tableone$fluidoverload_7 <- factor(data_tableone$fluidoverload_7, levels = c(1,0), labels = c("Yes", "No"))
data_tableone$fluidoverload_10 <- factor(data_tableone$fluidoverload_10, levels = c(1,0), labels = c("Yes", "No"))
data_tableone$SOFA_score <- as.integer(data_tableone$SOFA_score)
data_tableone$invasive_ventilation <- factor(data_tableone$invasive_ventilation, levels = c(1,0), labels = c("Yes", "No"))

# Diagnosis review classifications (HiRID patients only, Amsterdam = NA)
data_tableone$has_aki_or_acute_on_chronic <- factor(data_tableone$has_aki_or_acute_on_chronic, levels = c(1,0), labels = c("Yes", "No"))
data_tableone$has_eskd <- factor(data_tableone$has_eskd, levels = c(1,0), labels = c("Yes", "No"))
data_tableone$has_sepsis <- factor(data_tableone$has_sepsis, levels = c(1,0), labels = c("Yes", "No"))

labelestolabel <- list(
  is_female = "Female Sex",
  age_at_admission = "Age (years)",
  height_at_admission = "Height (cm)",
  weight_at_admission = "Weight (kg)",
  BMI = "Body Mass Index (kg/m^2)",
  emergency_admission = "Emergency Admission",
  adm_apache_group = "Reason for ICU-Admission",
  apache_score = "APACHE II Score at ICU-Admission",
  SOFA_score = "SOFA Score at start of CRRT",
  invasive_ventilation = "Invasive Ventilation at Start of CRRT",
  has_aki_or_acute_on_chronic = "Acute Kidney Injury (AKI)",
  has_eskd = "End-Stage Kidney Disease (ESKD)",
  has_sepsis = "Sepsis",
  length_of_stay = "ICU Length of Stay (days)",
  icu_death = "ICU Death",
  death_28d = "28-day Death",
  fluidoverload = "Fluid Balance before CRRT (L)",
  positive_fluidoverload = "Positive Fluid Balance before CRRT:",
  fluidoverload_5 = "Fluid Overload ≥ 5% (kg body weight)",
  fluidoverload_7 = "Fluid Overload ≥ 7% (kg body weight)",
  fluidoverload_10 = "Fluid Overload ≥ 10% (kg body weight)",
  vm2201_idx_first = "Noradrenaline at Start of CRRT (mcg/kg/min)",
  vm2105_first = "Lactate at Start of CRRT (mmol/l)"
)

# Build table
Table1 <- data_tableone %>% dplyr::select(source,
                                          is_female,age_at_admission,height_at_admission,weight_at_admission,BMI,
                                          emergency_admission,adm_apache_group,apache_score,SOFA_score,invasive_ventilation,
                                          vm2201_idx_first, vm2105_first,
                                          has_aki_or_acute_on_chronic, has_eskd, has_sepsis,
                                          fluidoverload, positive_fluidoverload, fluidoverload_5, fluidoverload_7, fluidoverload_10,
                                          length_of_stay, icu_death, death_28d) %>% 
  tbl_summary(
    by = source,
    label = labelestolabel,
    missing = "no", 
    #sort = all_categorical() ~ "frequency",
    statistic = list(age_at_admission ~ "{median} ({p25}-{p75})",
                     height_at_admission ~ "{median} ({p25}-{p75})",
                     weight_at_admission ~ "{median} ({p25}-{p75})",
                     BMI ~ "{median} ({p25}-{p75})",
                     apache_score ~ "{median} ({p25}-{p75})",
                     SOFA_score ~ "{median} ({p25}-{p75})",
                     vm2105_first ~ "{median} ({p25}-{p75})",
                     vm2201_idx_first ~ "{median} ({p25}-{p75})",
                     length_of_stay ~ "{median} ({p25}-{p75})",
                     fluidoverload ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ c(1, 1, 1), # Default: 1 decimal for median, p25, p75 or mean, sd
      height_at_admission ~ c(0, 0, 0),
      weight_at_admission ~ c(0, 0, 0),
      apache_score ~ c(0, 0, 0),
      SOFA_score ~ c(0, 0, 0),
      all_categorical() ~ c(0, 1) # n as integer, % with 1 decimal
    )) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Database**") %>% 
  add_overall()


Table1

Table1_gt <- as_gt(Table1)
gtsave(data = Table1_gt, path = glue("{R_output_root}"), filename = "Table_1.docx")

ft <- as_flex_table(Table1)
doc <- read_docx() %>%  body_add_flextable(value = ft)
#print(doc, target = glue("{R_output_root}"/Table_1_2.docx"))


#################################################################################
##### eTable 1 ####
#################################################################################

## Generates the same Table but stratified by Mean Ultrafiltration Group

data_e_tableone <- inner_join(data_tableone, UF_and_FB %>% dplyr::select(patid, mean_UFnet_bin), by="patid")
data_e_tableone$mean_UFnet_bin <- factor(data_e_tableone$mean_UFnet_bin, levels = c("high (>1.75)", "moderate (1.01-1.75)", "low (<1.01)", "zero"), labels = c("high (>1.75)", "moderate (1.01-1.75)", "low (0.01-1.0)", "zero (0)"))

eTable1 <- data_e_tableone %>% dplyr::select(mean_UFnet_bin,
                                           is_female,age_at_admission,height_at_admission,weight_at_admission,BMI,
                                           emergency_admission,adm_apache_group,apache_score,SOFA_score,invasive_ventilation,
                                           vm2201_idx_first, vm2105_first,
                                           has_aki_or_acute_on_chronic, has_eskd, has_sepsis,
                                           fluidoverload, positive_fluidoverload, fluidoverload_5, fluidoverload_7, fluidoverload_10,
                                           length_of_stay, icu_death, death_28d) %>% 
  tbl_summary(
    by = mean_UFnet_bin,
    label = labelestolabel,
    missing = "no", 
    sort = all_categorical() ~ "frequency",
    statistic = list(age_at_admission ~ "{median} ({p25}-{p75})",
                     height_at_admission ~ "{median} ({p25}-{p75})",
                     weight_at_admission ~ "{median} ({p25}-{p75})",
                     BMI ~ "{median} ({p25}-{p75})",
                     apache_score ~ "{median} ({p25}-{p75})",
                     SOFA_score ~ "{median} ({p25}-{p75})",
                     vm2105_first ~ "{median} ({p25}-{p75})",
                     vm2201_idx_first ~ "{median} ({p25}-{p75})",
                     length_of_stay ~ "{median} ({p25}-{p75})",
                     fluidoverload ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ c(1, 1, 1), # Default: 1 decimal for median, p25, p75 or mean, sd
      height_at_admission ~ c(0, 0, 0),
      weight_at_admission ~ c(0, 0, 0),
      apache_score ~ c(0, 0, 0),
      SOFA_score ~ c(0, 0, 0),
      all_categorical() ~ c(0, 1) # n as integer, % with 1 decimal
    )) %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Ultrafiltration Group of Mean Net Ultrafiltration (ml/kg/h)**") %>% 
  add_overall() 

eTable1

eTable1_gt <- as_gt(eTable1)
gtsave(eTable1_gt, filename = glue("{R_output_root}/eTable_1.docx"))
