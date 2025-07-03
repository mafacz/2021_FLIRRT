################################################################################

library(tidyverse)
library(ggplot2)
library(dplyr)
library(glue)
library(patchwork)

################################################################################

# Filter outliers to ±2 SD
filter_outliers <- function(df, var) {
  x <- df[[var]]
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  df %>% filter(!is.na(.data[[var]])) %>%
    filter(.data[[var]] > mean_x - 2 * sd_x,
           .data[[var]] < mean_x + 2 * sd_x)
}

################################################################################

# Compute shared x-axis limits for harmonized histograms
compute_shared_xlim <- function(files, vars, outlier_filter = TRUE, padding = 0.05) {
  xlims <- list()
  for (var in vars) {
    all_vals <- c()
    for (f in files) {
      df <- read.csv(f)
      if (outlier_filter) {
        df <- filter_outliers(df, var)
      }
      all_vals <- c(all_vals, df[[var]])
    }
    min_val <- min(all_vals, na.rm = TRUE)
    max_val <- max(all_vals, na.rm = TRUE)
    range_pad <- padding * (max_val - min_val)
    xlims[[var]] <- c(min_val - range_pad, max_val + range_pad)
  }
  return(xlims)
}

################################################################################

# Histogram plotting function for one database
histogram_function_summary <- function(database = c("HiRID", "AmsterdamUMCDb", "Total"),
                                       xlim_uf = NULL, xlim_uf_idx = NULL, 
                                       xlim_fb = NULL, xlim_fb_pre = NULL) {
  database <- match.arg(database)
  
  file_prefix <- glue("C:/Programming/FLIRRT/FLIRRT_preprocessing/Final/")
  
  suffix <- database
  stay_info <- read.csv(glue("{file_prefix}stays_fused_{database}.csv"))
  regular <- read.csv(glue("{file_prefix}regular_UFperkg_{database}.csv"))
  
  # Outlier-filtered data
  uf_clean <- filter_outliers(regular, "vm5010")
  uf_idx_clean <- filter_outliers(regular, "vm5010_idx")
  fb_pre_clean <- filter_outliers(stay_info, "fluidoverload")
  fb_clean <- filter_outliers(regular, "dm_balancerate_h")
  
  # UF Histograms
  H1_UF <- ggplot(uf_clean, aes(x = vm5010)) +
    geom_histogram(bins = 30, fill = "#F8766D", alpha = 0.6) +
    labs(subtitle = glue("{suffix} Cohort"),
         x = "Ultrafiltration rate (ml/h)") +
    (if (!is.null(xlim_uf)) xlim(xlim_uf[1], xlim_uf[2]) else NULL)
  
  H2_UF <- ggplot(uf_idx_clean, aes(x = vm5010_idx)) +
    geom_histogram(bins = 30, fill = "#F8766D", alpha = 0.6) +
    labs(subtitle = glue("{suffix} Cohort"),
         x = "Ultrafiltration rate (ml/kg/h)") +
    (if (!is.null(xlim_uf_idx)) xlim(xlim_uf_idx[1], xlim_uf_idx[2]) else NULL)
  
  UF_plot <- (H1_UF | H2_UF) 
  
  # Fluid Balance Histograms
  H1_FB_pre <- ggplot(fb_pre_clean, aes(x = fluidoverload)) +
    geom_histogram(bins = 100, fill = "#00BFC4", alpha = 0.6) +
    labs(subtitle = glue("{suffix} Cohort"),
         x = "Cumulative Fluid Balance before Start of 
         Continuous Renal Replacement Therapy (L)") +
    (if (!is.null(xlim_fb_pre)) xlim(xlim_fb_pre[1], xlim_fb_pre[2]) else NULL)
  
  H2_FB <- ggplot(fb_clean, aes(x = dm_balancerate_h)) +
    geom_histogram(bins = 100, fill = "#00BFC4", alpha = 0.6) +
    labs(subtitle = glue("{suffix} Cohort"),
         x = "Hourly Fluid Balance Change (mL/h)") +
    (if (!is.null(xlim_fb)) xlim(xlim_fb[1], xlim_fb[2]) else NULL)
  
  FB_plot <- (H1_FB_pre | H2_FB)
  
  list(UF_plot = UF_plot, FB_plot = FB_plot)
}

################################################################################

# Calculate x-limits for harmonized plots
file_prefix <- "C:/Programming/FLIRRT/FLIRRT_preprocessing/Final/"
regular_files <- list(
  HiRID = paste0(file_prefix, "regular_UFperkg_HiRID.csv"),
  AmsterdamUMCDb = paste0(file_prefix, "regular_UFperkg_AmsterdamUMCDb.csv")
)
stay_info_files <- list(
  HiRID = paste0(file_prefix, "stays_fused_HiRID.csv"),
  AmsterdamUMCDb = paste0(file_prefix, "stays_fused_AmsterdamUMCDb.csv")
)

uf_xlims <- compute_shared_xlim(regular_files, c("vm5010", "vm5010_idx"))
fb_xlims <- compute_shared_xlim(regular_files, c("dm_balancerate_h"))
fb_pre_xlims <- compute_shared_xlim(stay_info_files, c("fluidoverload"))


# Calculate plots for each database
plots_Total <- histogram_function_summary("Total",
                                          xlim_uf = uf_xlims$vm5010,
                                          xlim_uf_idx = uf_xlims$vm5010_idx,
                                          xlim_fb = c(-800, 800),
                                          xlim_fb_pre = fb_pre_xlims$fluidoverload)
plots_HiRID <- histogram_function_summary("HiRID",
                                          xlim_uf = uf_xlims$vm5010,
                                          xlim_uf_idx = uf_xlims$vm5010_idx,
                                          xlim_fb = c(-800, 800),
                                          xlim_fb_pre = fb_pre_xlims$fluidoverload)
plots_Amsterdam <- histogram_function_summary("AmsterdamUMCDb",
                                              xlim_uf = uf_xlims$vm5010,
                                              xlim_uf_idx = uf_xlims$vm5010_idx,
                                              xlim_fb = c(-800, 800),
                                              xlim_fb_pre = fb_pre_xlims$fluidoverload)

# Plot combined UF
combined_UF_plot <- (plots_Total$UF_plot / plots_HiRID$UF_plot / plots_Amsterdam$UF_plot) 
combined_UF_plot
ggsave(filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 1a.png",
       width = 6, height = 8)

# Plot combined Fluid Balance
combined_FB_plot <- (plots_Total$FB_plot / plots_HiRID$FB_plot / plots_Amsterdam$FB_plot)
combined_FB_plot
ggsave(filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 1b.png",
       width = 6, height = 8)

eFigure1 <- (plots_Total$UF_plot / plots_HiRID$UF_plot / plots_Amsterdam$UF_plot/plots_Total$FB_plot / plots_HiRID$FB_plot / plots_Amsterdam$FB_plot) 
ggsave(filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 1.png",
       width = 8, height = 12)

