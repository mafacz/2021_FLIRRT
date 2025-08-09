library(tidyverse)
library(gt)
library(gtsummary)
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

#################################################################################
file_prefix <- file.path(output_root, "Final")

regular <- read.csv(glue("{file_prefix}/regular_UFperkg_Total.csv"))

summary_table <- regular %>%
  group_by(patid) %>%
  summarise(max_session_length = max(session_length, na.rm = TRUE)) %>%
  summarise(
    n_patients = n(),
    total_days = sum(max_session_length, na.rm = TRUE) / 60 / 24
  )

cat(sprintf(
  "%d patients received CRRT for a total of %.1f days (based on the longest session per patient).",
  summary_table$n_patients,
  summary_table$total_days
))
