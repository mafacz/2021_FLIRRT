library(tidyverse)
library(patchwork)
library(ggplot2)
library(ggExtra)
library(readr)
library(glue)
library(stringr)
library(rlang)
library(cowplot)

## Heatmap function ##

plot_mortality_heatmap <- function(data, 
                                   uf_bin_col = mean_UFnet_bin, 
                                   fb_bin_col = mean_FB_bin,
                                   uf_lower_thresh = 1.01,
                                   uf_upper_thresh = 1.75,
                                   fb_neutral_limit = 20.833,
                                   fb_negative_limit = 62.5,
                                   x_title = "Mean Change of Fluid Balance (ml/h)",
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
    labs(x = x_title,
         y = y_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

####################################################################################
####################################################################################
# Function to generate heatmaps for a given database

generate_all_heatmaps <- function(database = c("HiRID", "AmsterdamUMCDb", "Total"),
                                  file_prefix = "C:/Programming/FLIRRT/FLIRRT_preprocessing/Final/",
                                  plot_output = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\") {
  
  database <- match.arg(database)
  
  # Load data
  UF_and_FB <- read.csv(glue("{file_prefix}Fluid_and_Ultrafiltration_{database}.csv")) %>% 
    dplyr::select(patid, 
                  mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                  UF_AUC, unlimited_UF_increase_24h,
                  mean_dm_balancerate_h, mean_dm_balancerate_h_48h, median_dm_balancerate_h,
                  mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, Q3_UFnet_bin,
                  mean_FB_bin, mean_FB_bin_48h, mean_FB_bin_FBneg, Q1_FB_bin)
  
  mortality <- read.csv(glue("{file_prefix}stays_fused_{database}.csv")) %>%
    dplyr::select(patid, outcome_death_28d)
  
  combined <- inner_join(UF_and_FB, mortality, by = "patid")
  
  # Generate
  heatmap_mean <- plot_mortality_heatmap(combined,
                                         uf_bin_col = mean_UFnet_bin,
                                         fb_bin_col = mean_FB_bin,
                                         y_title = glue("Mean Net Ultrafiltration Rate 
                                                        (ml/kg/h)"))
  heatmap_Q3 <- plot_mortality_heatmap(combined,
                                       uf_bin_col = Q3_UFnet_bin,
                                       fb_bin_col = mean_FB_bin,
                                       y_title = glue("Upper Quartile of Net Ultrafiltration Rate 
                                                      (ml/kg/h)"))
  heatmap_mean_48h <- plot_mortality_heatmap(combined,
                                         uf_bin_col = mean_UFnet_bin_48h,
                                         fb_bin_col = mean_FB_bin_48h,
                                         y_title = glue("Mean Net Ultrafiltration Rate 
                                                        (ml/kg/h) in first 48h"),
                                         x_title = glue("Mean Change of Fluid Balance 
                                                        (ml/h) in first 48h"))
  heatmap_mean_UFpos <- plot_mortality_heatmap(combined,
                                       uf_bin_col = mean_UFnet_bin_UFpos,
                                       fb_bin_col = mean_FB_bin,
                                       y_title = glue("Mean Net Ultrafiltration Rate 
                                                      (ml/kg/h) when Ultrafitration > 0"))
  heatmap_mean_Q1_FB <- plot_mortality_heatmap(combined,
                                               uf_bin_col = mean_UFnet_bin,
                                               fb_bin_col = Q1_FB_bin,
                                               y_title = glue("MeanNet Ultrafiltration Rate 
                                                              (ml/kg/h)"),
                                               x_title = glue("Upper Quartile of Change 
                                                              of Fluid Balance (ml/h)"))  
  heatmap_mean_FBneg <- plot_mortality_heatmap(combined,
                                       uf_bin_col = mean_UFnet_bin,
                                       fb_bin_col = mean_FB_bin_FBneg,
                                       y_title = glue("Mean Net Ultrafiltration Rate 
                                                      (ml/kg/h)"),
                                       x_title = glue("Mean Change of Fluid Balance 
                                                      (ml/h) when Fluid Balance Change < 0"))

  
  results <- list(heatmap_mean, heatmap_Q3, heatmap_mean_48h, heatmap_mean_Q1_FB, heatmap_mean_UFpos, heatmap_mean_FBneg)
  return(results)
}

heatmaps <- generate_all_heatmaps(database="Total")
heatmap_mean <- heatmaps[[1]]
heatmap_Q3 <- heatmaps[[2]]
heatmap_mean_48h <- heatmaps[[3]]
heatmap_mean_Q1_FB <- heatmaps[[4]]
heatmap_mean_UFpos <- heatmaps[[5]]
heatmap_mean_FBneg <- heatmaps[[6]]

ggsave(plot= heatmap_mean_48h, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3a\\eFigure 3a_48h.png", bg="white", width = 6, height = 6)
ggsave(plot= heatmap_mean_Q1_FB, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3a\\eFigure 3a_Q1_FB.png", bg="white", width = 6, height = 6)
ggsave(plot= heatmap_mean_UFpos, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3a\\eFigure 3a_UFpos.png", bg="white", width = 6, height = 6)
ggsave(plot= heatmap_mean_FBneg, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3a\\eFigure 3a_FB_neg.png", bg="white", width = 6, height = 6)


################################################################################
## Heatmap for Subgroups: Database

heatmaps_HiRID <- generate_all_heatmaps(database="HiRID")
heatmap_mean_HiRID <- heatmaps_HiRID[[1]]
heatmap_Q3_HiRID <- heatmaps_HiRID[[2]]
ggsave(plot= heatmap_mean_HiRID, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_HiRID.png", bg="white", width = 6, height = 6)
ggsave(plot= heatmap_Q3_HiRID, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_Q3_HiRID.png", bg="white", width = 6, height = 6)

heatmaps_AmsterdamUMCdb <- generate_all_heatmaps(database = "AmsterdamUMCDb")
heatmap_mean_AmsterdamUMCdb <- heatmaps_AmsterdamUMCdb[[1]]
heatmap_Q3_AmsterdamUMCdb <- heatmaps_AmsterdamUMCdb[[2]]
ggsave(plot= heatmap_mean_AmsterdamUMCdb, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_AmsterdamUMCdb.png", bg="white", width = 6, height = 6)
ggsave(plot= heatmap_Q3_AmsterdamUMCdb, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_Q3_AmsterdamUMCdb.png", bg="white", width = 6, height = 6)



##################################################################################
##################################################################################
## Load data

file_prefix <- "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\"

UF_and_FB <- read.csv(glue("{file_prefix}Fluid_and_Ultrafiltration_Total.csv")) %>% 
  dplyr::select(patid,
                mean_vm5010_idx, mean_vm5010_idx_48h, mean_vm5010_idx_UFpos, Q3_vm5010_idx,
                UF_AUC, unlimited_UF_increase_24h,
                mean_dm_balancerate_h, mean_dm_balancerate_h_48h, mean_dm_balancerate_h_FBneg, median_dm_balancerate_h, Q1_dm_balancerate_h,
                mean_UFnet_bin, mean_UFnet_bin_48h, mean_UFnet_bin_UFpos, mean_FB_bin, mean_FB_bin_48h,mean_FB_bin_FBneg, Q1_FB_bin)
stay_info <- read.csv(glue("{file_prefix}stays_fused_Total.csv"))
Lab_values <- read.csv(glue("{file_prefix}Lab_values_reg_Total.csv")) %>% dplyr::select(patid, Noradrenalin_increase_0.1_mcgkgmin,Lactate_increase_2,Lactate_increase_4)
combined <- inner_join(UF_and_FB, stay_info, by="patid")
combined <- inner_join(combined, Lab_values, by="patid")

################################################################################
## Heatmap for Subgroups: Sex

Heatmap_females <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(gender == "F")), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                   x_title = "Mean Change of Fluid Balance (ml/h)",
                                   y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_females
ggsave(plot= Heatmap_females, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Sex_F.png", bg="white", width = 6, height = 6)

Heatmap_males <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(gender == "M")), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                          x_title = "Mean Change of Fluid Balance (ml/h)",
                                          y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_males
ggsave(plot= Heatmap_males, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Sex_M.png", bg="white", width = 6, height = 6)

################################################################################
## Heatmap for Subgroups: FLuidoverload >= 10% pre-CRRT

Heatmap_fluidoverload_10 <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(fluidoverload >= 10)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                   x_title = "Mean Change of Fluid Balance (ml/h)",
                                                   y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_fluidoverload_10
ggsave(plot= Heatmap_fluidoverload_10, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Fluidoverload10.png", bg="white", width = 6, height = 6)

Heatmap_nofluidoverload <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(fluidoverload < 10)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                     x_title = "Mean Change of Fluid Balance (ml/h)",
                                                     y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_nofluidoverload
ggsave(plot= Heatmap_nofluidoverload, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_NoFluidoverload.png", bg="white", width = 6, height = 6)

################################################################################
## Heatmap for Subgroups: Noradrenalin

Heatmap_NA_increase <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Noradrenalin_increase_0.1_mcgkgmin == 1)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                              x_title = "Mean Change of Fluid Balance (ml/h)",
                                              y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_NA_increase
ggsave(plot= Heatmap_NA_increase, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Noradre_increase0.1.png", bg="white", width = 6, height = 6)

Heatmap_NA_noincrease <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Noradrenalin_increase_0.1_mcgkgmin == 0)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                x_title = "Mean Change of Fluid Balance (ml/h)",
                                                y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_NA_noincrease
ggsave(plot= Heatmap_NA_noincrease, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Noradre_noincrease0.1.png", bg="white", width = 6, height = 6)

################################################################################
## Heatmap for Subgroups: Lactate

Heatmap_Lactate2_increase <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Lactate_increase_2 == 1)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                              x_title = "Mean Change of Fluid Balance (ml/h)",
                                              y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_Lactate2_increase
ggsave(plot= Heatmap_Lactate2_increase, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Lactate2_increase.png", bg="white", width = 6, height = 6)

Heatmap_Lactate2_noincrease <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Lactate_increase_2 == 0)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                    x_title = "Mean Change of Fluid Balance (ml/h)",
                                                    y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_Lactate2_noincrease
ggsave(plot= Heatmap_Lactate2_noincrease, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Lactate2_noincrease.png", bg="white", width = 6, height = 6)

Heatmap_Lactate4_increase <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Lactate_increase_2 == 1)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                    x_title = "Mean Change of Fluid Balance (ml/h)",
                                                    y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_Lactate4_increase
ggsave(plot= Heatmap_Lactate4_increase, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Lactate4_increase.png", bg="white", width = 6, height = 6)

Heatmap_Lactate4_noincrease <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Lactate_increase_2 == 0)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                      x_title = "Mean Change of Fluid Balance (ml/h)",
                                                      y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_Lactate4_noincrease
ggsave(plot= Heatmap_Lactate4_noincrease, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Lactate4_noincrease.png", bg="white", width = 6, height = 6)

################################################################################
## Heatmap for Subgroups: ANURIE

Heatmap_Anuria_onset <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Anuria_onset == 1)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                    x_title = "Mean Change of Fluid Balance (ml/h)",
                                                    y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_Anuria_onset
ggsave(plot= Heatmap_Anuria_onset, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Anuria_onset.png", bg="white", width = 6, height = 6)

Heatmap_no_Anuria_onset <- plot_mortality_heatmap(data = (combined %>% dplyr::filter(Anuria_onset == 0)), uf_bin_col = mean_UFnet_bin,fb_bin_col = mean_FB_bin,
                                                      x_title = "Mean Change of Fluid Balance (ml/h)",
                                                      y_title = "Mean Net Ultrafiltration Rate (ml/kg/h)")
Heatmap_no_Anuria_onset
ggsave(plot= Heatmap_no_Anuria_onset, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 3b\\eFigure 3b_mean_Anuria_noonset.png", bg="white", width = 6, height = 6)

