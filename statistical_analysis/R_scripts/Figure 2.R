library(tidyverse)
library(patchwork)
library(ggplot2)
library(ggExtra)
library(readr)
library(glue)
library(stringr)
library(rlang)
library(cowplot)
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
file_prefix <- file.path(output_root, "Final")

################################################################################
################################################################################
## Plot Spline Plots for UF and FB

function_filter_2SD <- function(dataframe, x_data) {
  x_col <- enquo(x_data)
  x_vals <- pull(dataframe, !!x_col)
  x_mean <- mean(x_vals, na.rm = TRUE)
  x_sd <- sd(x_vals, na.rm = TRUE)
  
  lower_limit <- x_mean - 2 * x_sd
  upper_limit <- x_mean + 2 * x_sd
  
  dataframe %>%
    filter((!!x_col) >= lower_limit,
           (!!x_col) <= upper_limit)
}

function_spline_plot <- function(dataframe, x_data, y_data = outcome_death_28d, x_title, y_title = "28-day mortality",
                                 wrap_width = 40, xlim_range = NULL, ylim_range = NULL) {
  # Filter data first
  filtered_df <- function_filter_2SD(dataframe, {{x_data}})
  
  plot <- filtered_df %>%
    ggplot(aes(x = {{x_data}}, y = {{y_data}})) +
    geom_smooth() +
    geom_point(alpha = 0.0) +
    labs(x = x_title, y = y_title)
  
  if (!is.null(xlim_range)) plot <- plot + xlim(xlim_range)
  if (!is.null(ylim_range)) plot <- plot + ylim(ylim_range)
  
  return(plot)
}

marginal_histogram <- function(plot) {
  ggMarginal(plot, type = "histogram", margins = c("x"))
}

function_spline_plot_dual <- function(dataframe, 
                                      x_data, 
                                      x_data2 = NULL, 
                                      y_data = outcome_death_28d, 
                                      x_title, 
                                      y_title = "28-day mortality",
                                      wrap_width = 40, 
                                      xlim_range = NULL, 
                                      ylim_range = NULL,
                                      label1 = "x_data", 
                                      label2 = "x_data2",
                                      histo_lim = NULL,
                                      tag = 'a') {
  
  # Filter both datasets using the same filter function
  df1 <- function_filter_2SD(dataframe, {{x_data}}) %>%
    mutate(x_value = {{x_data}}, group = label1)
  
  plot_data <- df1
  
  # If second x_data is provided
  if (!is.null(enquo(x_data2))) {
    df2 <- function_filter_2SD(dataframe, {{x_data2}}) %>%
      mutate(x_value = {{x_data2}}, group = label2)
    plot_data <- bind_rows(df1, df2)
  }
  
  # Build plot
  plot <- ggplot(plot_data, aes(x = x_value, y = {{y_data}}, color = group)) +
    geom_smooth() +
    geom_point(alpha = 0) +
    labs(x = x_title, y = y_title, color = NULL)
  
  if (!is.null(xlim_range)) plot <- plot + xlim(xlim_range)
  if (!is.null(ylim_range)) plot <- plot + ylim(ylim_range)
  
  # Histogram plot above
  hist_plot <- ggplot(plot_data, aes(x = x_value, fill = group)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 70) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    ylim({{histo_lim}}) + labs(tag = tag)
  if (!is.null(xlim_range)) hist_plot <- hist_plot + xlim(xlim_range)
  if (!is.null(y_title)) {
    hist_plot <- hist_plot + labs(y = "Count")
  } else {
    hist_plot <- hist_plot + theme(axis.title.y = element_blank())
  }
  
  # Combine plots
  combined_plot <- hist_plot / plot + plot_layout(heights = c(1, 2))
  return(combined_plot)
  
}

################################################################################
################################################################################
## Load data

UF_and_FB <- read.csv(glue("{file_prefix}/Fluid_and_Ultrafiltration_Total.csv")) %>% 
  dplyr::select(patid, 
                mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                fluidoverload, UF_AUC, unlimited_UF_increase_24h,
                mean_dm_balancerate_h, mean_dm_balancerate_h_48h, mean_dm_balancerate_h_FBneg, median_dm_balancerate_h, Q1_dm_balancerate_h,
                mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, mean_FB_bin, mean_FB_bin_48h,mean_FB_bin_FBneg, Q1_FB_bin)
mortality <- read.csv(glue("{file_prefix}/stays_fused_Total.csv"))
combined <- inner_join(UF_and_FB, mortality, by="patid")
patient_numbers <- nrow(combined)

## Spline plots of UF: mean and 3.IQR
ylim = c(0,1.2)
xlim_UF = c(0,3.3)
UF_plot <- function_spline_plot_dual(dataframe = combined, x_data = mean_vm5010_idx, x_data2 = Q3_vm5010_idx, 
                                     x_title = "Net Ultrafiltration Rate (ml/kg/h)", label1 = "Mean", label2 = "Upper Quartile", 
                                     xlim_range = xlim_UF, ylim_range = ylim, histo_lim = c(0,70), tag='a')


## Spline plots of FB: mean and 1.IQR
xlim_FB = c(-500,200)
FB_plot <- function_spline_plot_dual(dataframe = combined, x_data = mean_dm_balancerate_h, x_data2 = Q1_dm_balancerate_h, 
                                     x_title = "Change of Fluid Balance (ml/h)", y_title = NULL, label1 = "Mean", label2 = "Upper Quartile", 
                                     xlim_range = xlim_FB, ylim_range = ylim, histo_lim = c(0,70), tag='b')

## Combine to one plot
Figure2ab <- (UF_plot | FB_plot)
ggsave(plot = Figure2ab, filename = glue("{R_output_root}/Figure 2ab.png"),
       width = 8, height = 4)

################################################################################
################################################################################
## Plot Heatmaps ##

## Heatmap function ##

plot_mortality_heatmap <- function(data, 
                                   uf_bin_col = mean_UFnet_bin, 
                                   fb_bin_col = mean_FB_bin,
                                   uf_lower_thresh = 1.01,
                                   uf_upper_thresh = 1.75,
                                   fb_neutral_limit = 20.833,
                                   fb_negative_limit = 62.5,
                                   x_title = "Mean Change of Fluid Balance [ml/h]",
                                   y_title = NULL) {
  
  data <- data %>% filter({{uf_bin_col}} != "NaN", {{fb_bin_col}} != "NaN") %>%
    group_by({{ uf_bin_col }}, {{ fb_bin_col }}) %>%
    summarise(mortality_rate = mean(outcome_death_28d, na.rm = TRUE),
              n_patients = n(), .groups = "drop")
  
  # Add deaths column
  data <- data %>%
    mutate(deaths = mortality_rate * n_patients)
  
  # Rename bin variables for consistent plotting
  data <- data %>%
    rename(uf_bin = {{ uf_bin_col }}, fb_bin = {{ fb_bin_col }})
  # Set bin orderings
  uf_levels <- c(glue("high (>{uf_upper_thresh})"),
                 glue("moderate ({uf_lower_thresh}-{uf_upper_thresh})"),
                 glue("low (<{uf_lower_thresh})"),
                 "zero", "NaN")
  fb_levels <- c(glue("strong_negative (< -{fb_negative_limit})"),
                 glue("light_negative (-{fb_negative_limit} - -{fb_neutral_limit})"),
                 glue("neutral (-{fb_neutral_limit} - {fb_neutral_limit})"),
                 glue("positive (>{fb_neutral_limit})"),
                 "NaN")
  data <- data %>%  mutate(uf_bin = factor(uf_bin, levels = uf_levels),
                           fb_bin = factor(fb_bin, levels = fb_levels))
  
  # Row and column totals
  row_totals <- data %>% group_by(uf_bin) %>%
    summarise(n_patients = sum(n_patients),
              deaths = sum(deaths),
              mortality_rate = deaths / n_patients,
              fb_bin = "Total", .groups = "drop")
  col_totals <- data %>% group_by(fb_bin) %>%
    summarise(n_patients = sum(n_patients),
              deaths = sum(deaths),
              mortality_rate = deaths / n_patients,
              uf_bin = "Total", .groups = "drop")
  grand_total <- tibble(uf_bin = "Total", fb_bin = "Total",
                        n_patients = sum(data$n_patients),
                        deaths = sum(data$deaths),
                        mortality_rate = sum(data$deaths) / sum(data$n_patients))
  
  # Combine with original data
  augmented <- bind_rows(data, row_totals, col_totals, grand_total)
  
  # Reapply factors with Total
  augmented <- augmented %>%
    mutate(uf_bin = factor(uf_bin, levels = c(uf_levels, "Total")),
           fb_bin = factor(fb_bin, levels = c(fb_levels, "Total")))
  
  # Create heatmap
  ggplot(augmented, aes(x = fb_bin, y = uf_bin, fill = mortality_rate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0("M: ", round(mortality_rate, 2), "\nN=", n_patients)),
              color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "#7570b3", name = "Mortality Rate", limits = c(0, 1)) +
    scale_x_discrete(labels = c("strong_negative (< -62.5)" = "Strong Negative (<-62.5)",
                                "light_negative (-62.5 - -20.833)" = "Light Negative (-62.5 - -20.84)",
                                "neutral (-20.833 - 20.833)" = "Neutral (-20.83 - 20.83)",
                                "positive (>20.833)" = "Positive (>20.83)")) + 
    scale_y_discrete(labels = c("zero" = "Zero (0)", 
                                "low (<1.01)" = "Low (0.01 - 1.0)",
                                "moderate (1.01-1.75)" = "Moderate (1.01 - 1.75)",
                                "high (>1.75)" = "High (>1.75)")) +
    labs(x = x_title, y = y_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

####################################################################################
####################################################################################
# Function to generate heatmaps for a given database

generate_all_heatmaps <- function(database = c("HiRID", "AmsterdamUMCDb", "Total")) {
  
  database <- match.arg(database)
  
  # Load data
  UF_and_FB <- read.csv(glue("{file_prefix}/Fluid_and_Ultrafiltration_{database}.csv")) %>% 
    dplyr::select(patid, 
                  mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                  fluidoverload, UF_AUC, unlimited_UF_increase_24h,
                  mean_dm_balancerate_h, mean_dm_balancerate_h_48h, median_dm_balancerate_h,
                  mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, Q3_UFnet_bin,
                  mean_FB_bin, mean_FB_bin_48h)
  
  mortality <- read.csv(glue("{file_prefix}/stays_fused_{database}.csv")) %>%
    dplyr::select(patid, outcome_death_28d)
  
  combined <- inner_join(UF_and_FB, mortality, by = "patid")
  
  # Generate
  heatmap_mean <- plot_mortality_heatmap(combined,
                                         uf_bin_col = mean_UFnet_bin,
                                         fb_bin_col = mean_FB_bin,
                                         y_title = glue("Mean Net Ultrafiltration Rate 
                                                        (ml/kg/h)")
  )
  heatmap_Q3 <- plot_mortality_heatmap(combined,
                                       uf_bin_col = Q3_UFnet_bin,
                                       fb_bin_col = mean_FB_bin,
                                       y_title = glue("Upper Quartile of Net Ultrafiltration Rate 
                                                      (ml/kg/h)"))
  results <- list(heatmap_mean, heatmap_Q3)
  return(results)
}

heatmaps <- generate_all_heatmaps(database="Total")
heatmap_mean <- heatmaps[[1]]
heatmap_Q3 <- heatmaps[[2]]
Figure2c <- (heatmap_mean / heatmap_Q3)
ggsave(plot = Figure2c, glue("{R_output_root}/Figure 2c.png"), height = 12, width = 8)


################################################################################
## Combine Heatmap and Spline plots :)

library(patchwork)


## Apply to heatmaps:
# Left heatmap: keep y-axis
heatmap_mean_clean <- heatmap_mean +
  theme(axis.title.y = element_text(),
        axis.text.y = element_text(),
        axis.ticks.y = element_line()) + labs(tag = 'c')

# Right heatmap: remove y-axis, add y-axis title
heatmap_Q3_clean <- heatmap_Q3 +
  theme(axis.title.y = element_text(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + labs(tag = 'd')

# Combine spline plots into a single row, full width
spline_combined <- (UF_plot | FB_plot) +
  plot_layout(widths = c(1, 1), guides = "collect")

# Combine heatmaps into a single row, same width alignment
heatmap_combined <- (heatmap_mean_clean | heatmap_Q3_clean) +
  plot_layout(widths = c(1, 1), guides = "collect")

# Stack spline plots and heatmaps vertically
Figure2 <- (spline_combined / heatmap_combined) + 
  plot_layout(heights = c(1.5, 1))

# Display and save
print(Figure2)
ggsave(
  plot = Figure2,
  filename = glue("{R_output_root}/Figure 2.tiff"),
  height = 12,
  width = 12,
  dpi = 300,
  units = "in",
  device = "tiff",
  compression = "lzw" # common for journal submissions
)

################################################################################

