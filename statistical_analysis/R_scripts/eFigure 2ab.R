library(tidyverse)
library(patchwork)
library(ggplot2)
library(ggExtra)
library(readr)
library(glue)
library(stringr)
library(rlang)
library(cowplot)

################################################################################
################################################################################

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
                                      legend_position = "bottom") {
  
  # Filter both datasets using the same filter function
  df1 <- function_filter_2SD(dataframe, {{x_data}}) %>%
    mutate(x_value = {{x_data}}, group = label1)
  
  plot_data <- df1
  
  # If second x_data is provided
  if (!missing(x_data2)) {
    df2 <- function_filter_2SD(dataframe, {{x_data2}}) %>%
      mutate(x_value = {{x_data2}}, group = label2)
    plot_data <- bind_rows(df1, df2)
  }
  
  # Build plot
  plot <- ggplot(plot_data, aes(x = x_value, y = {{y_data}}, color = group)) +
    geom_smooth() +
    geom_point(alpha = 0) +
    labs(x = x_title, y = y_title, color = NULL) + 
    theme(legend.position = {{legend_position}},  legend.box = "vertical", legend.direction = "vertical")
  
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
    labs(y = "Count") +
    ylim({{histo_lim}})
  if (!is.null(xlim_range)) hist_plot <- hist_plot + xlim(xlim_range)
  
  # Combine plots
  combined_plot <- hist_plot / plot + plot_layout(heights = c(1, 2))
  return(combined_plot)
  
}

function_spline_plot_three_groups <- function(dataframe,
                                              x_var,              # variable for x-axis (unquoted)
                                              group_var,          # grouping column (unquoted), e.g. source or sex
                                              y_var = outcome_death_28d,  # outcome variable (unquoted)
                                              x_label = NULL,
                                              y_label = "28-day mortality",
                                              legend_labels = NULL, # if NULL, auto-generate labels
                                              legend_position = "bottom",
                                              bins = 70,
                                              filter_function = function_filter_2SD,
                                              show_overall = TRUE,
                                              show_overall_hist = FALSE) {
  
  # Capture quosures for tidy eval
  x_var <- enquo(x_var)
  group_var <- enquo(group_var)
  y_var <- enquo(y_var)
  
  # Extract the grouping variable values
  group_levels <- dataframe %>% 
    pull(!!group_var) %>% 
    unique() %>% 
    na.omit() %>% 
    as.character()
  
  # Check if group has exactly 2 groups for plotting
  if(length(group_levels) != 2){
    stop("This function currently supports grouping variables with exactly 2 unique groups.")
  }
  
  # If legend_labels not provided, auto-generate with "Overall" + group levels
  if(is.null(legend_labels)){
    legend_labels <- c("Overall", group_levels[1], group_levels[2])
  } else {
    # Validate length
    if(length(legend_labels) != 3){
      stop("legend_labels must be length 3: Overall + 2 groups")
    }
  }
  
  # Apply optional filter function or identity
  if(!is.null(filter_function)){
    dataframe <- filter_function(dataframe, !!x_var)
  }
  
  # Prepare data for overall and two groups
  df_overall <- dataframe %>%
    mutate(group = legend_labels[1],
           x_value = !!x_var,
           y_value = !!y_var) %>%
    select(x_value, y_value, group)
  
  df_group1 <- dataframe %>%
    filter(!!group_var == group_levels[1]) %>%
    mutate(group = legend_labels[2],
           x_value = !!x_var,
           y_value = !!y_var) %>%
    select(x_value, y_value, group)
  
  df_group2 <- dataframe %>%
    filter(!!group_var == group_levels[2]) %>%
    mutate(group = legend_labels[3],
           x_value = !!x_var,
           y_value = !!y_var) %>%
    select(x_value, y_value, group)
  
  # Combine all groups
  plot_data <- bind_rows(df_overall, df_group1, df_group2)
  
  # Filter out overall group if requested
  if(!show_overall){
    plot_data <- plot_data %>% filter(group != legend_labels[1])
  }
  
  # Convert group to factor for plot order and consistent colors
  plot_data$group <- factor(plot_data$group, levels = legend_labels)
  
  # Create a manual color scale with consistent colors
  color_values <- c("#7570b3", "#00BFC4", "#F8766D") # Colors can be changed :)
  names(color_values) <- legend_labels
  
  # Plot spline curves
  p_curve <- ggplot(plot_data, aes(x = x_value, y = y_value, color = group)) +
    geom_smooth(se = TRUE, method = "loess") +
    geom_point(alpha = 0) + # invisible points to assist smoothing
    labs(x = ifelse(is.null(x_label), quo_name(x_var), x_label),
         y = y_label,
         color = NULL) +
    ylim(c(0,1)) +
    scale_color_manual(values = color_values) +
    theme_minimal() +
    theme(legend.position = legend_position,legend.box = "vertical", legend.direction = "vertical")
  
  # Histogram of overall data
  p_hist_overall <- ggplot(df_overall, aes(x = x_value)) +
    geom_histogram(fill = "#7570b3", alpha = 0.3, bins = bins) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Count (Overall)") 
  
  # Histogram data: exclude overall if user wants
  if(show_overall_hist){
    hist_data <- plot_data
  } else {
    hist_data <- plot_data %>% filter(group != legend_labels[1])
  }
  
  # Histogram plot with matching fill colors
  p_hist_group <- ggplot(hist_data, aes(x = x_value, fill = group)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = bins) +
    scale_fill_manual(values = color_values) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Count")
  
  # Combine histogram + curve plot vertically
  combined_plot <- wrap_elements(p_hist_overall) / 
    wrap_elements(p_hist_group) / 
    wrap_elements(p_curve) + 
    plot_layout(heights = c(1, 1, 3))
  
  return(combined_plot)
}

function_spline_plot_groups <- function(dataframe,
                                        x_var,              # variable for x-axis (unquoted)
                                        group_var,          # grouping column (unquoted), e.g. source or sex
                                        y_var = outcome_death_28d,  # outcome variable (unquoted)
                                        x_label = NULL,
                                        y_label = "28-day mortality",
                                        legend_labels = NULL, # custom labels, must match number of groups (+1 if overall)
                                        legend_position = "bottom",
                                        bins = 70,
                                        filter_function = function_filter_2SD,
                                        show_overall = TRUE, #plot line of totalgroup 
                                        show_overall_hist = FALSE) {
  
  # Capture quosures for tidy eval
  x_var <- enquo(x_var)
  group_var <- enquo(group_var)
  y_var <- enquo(y_var)
  
  # Apply optional filter function or identity
  if (!is.null(filter_function)) {
    dataframe <- filter_function(dataframe, !!x_var)
  }
  
  # Get unique group levels
  group_levels <- dataframe %>%
    pull(!!group_var) %>%
    unique() %>%
    na.omit() %>%
    as.character()
  
  # Auto-generate legend labels if not provided
  if (is.null(legend_labels)) {
    legend_labels <- if (show_overall) c("Overall", group_levels) else group_levels
  } else {
    expected_length <- if (show_overall) length(group_levels) + 1 else length(group_levels)
    if (length(legend_labels) != expected_length) {
      stop(paste0("legend_labels must have length ", expected_length, ": one per group", if (show_overall) " + 1 for overall." else "."))
    }
  }
  
  # Set up color palette
  palette <- c("#7570b3", "#00BFC4", "#F8766D", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")
  if (length(legend_labels) > length(palette)) {
    palette <- scales::hue_pal()(length(legend_labels))
  }
  color_values <- setNames(palette[1:length(legend_labels)], legend_labels)
  
  # Create data frames for each group
  plot_data <- purrr::map_dfr(group_levels, function(g, i = NULL) {
    dataframe %>%
      filter(!!group_var == g) %>%
      mutate(group = legend_labels[which(group_levels == g) + if (show_overall) 1 else 0],
             x_value = !!x_var,
             y_value = !!y_var) %>%
      select(x_value, y_value, group)
  })
  
  # Add overall if needed
  if (show_overall) {
    df_overall <- dataframe %>%
      mutate(group = legend_labels[1],
             x_value = !!x_var,
             y_value = !!y_var) %>%
      select(x_value, y_value, group)
    plot_data <- bind_rows(df_overall, plot_data)
  }
  
  # Ensure correct factor levels
  plot_data$group <- factor(plot_data$group, levels = legend_labels)
  
  # Spline curve plot
  p_curve <- ggplot(plot_data, aes(x = x_value, y = y_value, color = group)) +
    geom_smooth(se = TRUE, method = "loess") + #Method of smoothing can be changed here
    geom_point(alpha = 0) +
    labs(x = ifelse(is.null(x_label), quo_name(x_var), x_label),
         y = y_label,
         color = NULL) +
    ylim(c(0, 1)) +
    scale_color_manual(values = color_values) +
    theme_minimal() +
    theme(legend.position = legend_position, legend.box = "vertical", legend.direction = "vertical")
  
  # Histogram of overall
  p_hist_overall <- NULL
  if (show_overall) {
    df_overall <- plot_data %>% filter(group == legend_labels[1])
    p_hist_overall <- ggplot(df_overall, aes(x = x_value)) +
      geom_histogram(fill = color_values[legend_labels[1]], alpha = 0.3, bins = bins) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(y = "Count (Overall)")
  }
  
  # Histogram of groups (excluding overall if necessary)
  hist_data <- if (show_overall_hist) {
    plot_data
  } else {
    plot_data %>% filter(group != legend_labels[1])
  }
  
  hist_data <- plot_data %>% filter(group != "Overall" | show_overall_hist)
  p_hist_groups <- ggplot(hist_data, aes(x = x_value, fill = group)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = bins) +
    scale_fill_manual(values = color_values) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Count")
  
  # Wrap plots
  wrapped <- list()
  if (!is.null(p_hist_overall)) wrapped <- append(wrapped, list(wrap_elements(p_hist_overall)))
  wrapped <- append(wrapped, list(wrap_elements(p_hist_groups), wrap_elements(p_curve)))
  
  # Combine all
  combined_plot <- patchwork::wrap_plots(wrapped, ncol = 1, heights = c(rep(1, length(wrapped) - 1), 3))
  
  return(combined_plot)
}

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
Lab_values <- read.csv(glue("{file_prefix}Lab_values_reg_Total.csv")) %>% dplyr::select(patid, Noradrenalin_increase_0.1_mcgkgmin,Lactate_increase_2,Lactate_increase_4,Anuria_onset)
stay_info <- read.csv(glue("{file_prefix}stays_fused_Total.csv"))
combined <- inner_join(UF_and_FB, stay_info, by="patid")
combined <- inner_join(combined, Lab_values, by="patid")

##################################################################################
## Spline plots of UF and FB: mean < 48h and mean when UF > 0 / FB < 0 respectively

xlim_UF = c(0,3.3)
UF_plot <- function_spline_plot_dual(dataframe = combined, x_data = mean_vm5010_idx_48h, x_data2 = mean_vm5010_idx_UFpos, 
                                     x_title = "Net Ultrafiltration Rate (ml/kg/h)", 
                                     label1 = glue("Mean of first 48h of Treatment"), 
                                     label2 = glue("Mean when net Ultrafiltration > 0"), 
                                     xlim_range = xlim_UF, histo_lim = c(0,70))
UF_plot <- UF_plot %>% wrap_elements()

## Spline plots of FB: mean and 1.IQR
xlim_FB = c(-520,300)
FB_plot <- function_spline_plot_dual(dataframe = combined, x_data = mean_dm_balancerate_h_48h, x_data2 = mean_dm_balancerate_h_FBneg, 
                                     x_title = "Change of Fluid Balance (ml/h)", label1 = "Mean of first 48h of Treatment", label2 = "Mean when Fluid Balance Change < 0", 
                                     xlim_range = xlim_FB, histo_lim = c(0,70))
FB_plot <- FB_plot %>% wrap_elements()

## Combine to one plot
eFigure2a <- (UF_plot | FB_plot)
eFigure2a
ggsave(plot = eFigure2a, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2a.png",
       width = 8, height = 4)

#############################################
## Subgroup plots: for database

Mean_UF_database <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_vm5010_idx, group_var = source, y_var = outcome_death_28d,
                                                     x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                                     legend_labels = c("Overall", "HiRID", "AmsterdamUMCdb"), 
                                                     legend_position = "bottom", bins = 70)
Mean_UF_database

Mean_FB_database <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_dm_balancerate_h, group_var = source, y_var = outcome_death_28d,
                                                      x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                                      legend_labels = c("Overall", "HiRID", "AmsterdamUMCdb"), 
                                                      legend_position = "bottom", bins = 70)
Mean_FB_database

combined_database <- Mean_UF_database %>% wrap_elements() | Mean_FB_database %>% wrap_elements()
ggsave(plot=combined_database, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_database.png",
       width = 8, height = 6)

#############################################
## Subgroup plots: for Sex

Mean_UF_Sex <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_vm5010_idx, group_var = gender, y_var = outcome_death_28d,
                                             x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                             legend_labels = c("Overall", "Female", "Male"), 
                                             legend_position = "bottom", bins = 70)
Mean_UF_Sex

Mean_FB_Sex <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_dm_balancerate_h, group_var = gender, y_var = outcome_death_28d,
                                                 x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                                 legend_labels = c("Overall", "Female", "Male"), 
                                                 legend_position = "bottom", bins = 70)
Mean_FB_Sex

combined_sex <- Mean_UF_Sex %>% wrap_elements() | Mean_FB_Sex %>% wrap_elements()
ggsave(plot=combined_sex, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_sex.png",
       width = 8, height = 6)

#############################################
## Subgroup plots: for Fluid Overload >= 10 %

Mean_UF_FO <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_vm5010_idx, group_var = fluidoverload_10, y_var = outcome_death_28d,
                                                x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                                legend_labels = c("Overall", "Fluid overload ≥ 10%", "No Fluid overload ≥ 10%"), 
                                                legend_position = "bottom", bins = 70)
Mean_UF_FO

Mean_FB_FO <- function_spline_plot_three_groups(dataframe = combined, x_var = mean_dm_balancerate_h, group_var = fluidoverload_10, y_var = outcome_death_28d,
                                                 x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                                 legend_labels = c("Overall", "Fluid overload ≥ 10%", "No Fluid overload ≥ 10%"), 
                                                 legend_position = "bottom", bins = 70)
Mean_FB_FO

combined_FO <- Mean_UF_FO %>% wrap_elements() | Mean_FB_FO %>% wrap_elements()
ggsave(plot=combined_FO, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_fluidoverload10.png",
       width = 8, height = 6)

#############################################
## Subgroup plots: for Noradreanlin increase 

##CAVE: Filtered out patients with Increased Noradreanlin at Start of CRRT

Noradre_df <- combined %>% filter(Noradrenalin_increase_0.1_mcgkgmin<2)

Mean_UF_NOR <- function_spline_plot_groups(dataframe = Noradre_df, x_var = mean_vm5010_idx, group_var = Noradrenalin_increase_0.1_mcgkgmin, y_var = outcome_death_28d,
                                           x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                           show_overall = TRUE,
                                           legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"),
                                           legend_position = "bottom", bins = 70)
Mean_UF_NOR

Mean_FB_NOR <- function_spline_plot_groups(dataframe = Noradre_df, x_var = mean_dm_balancerate_h, group_var = Noradrenalin_increase_0.1_mcgkgmin, y_var = outcome_death_28d,
                                           x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                           legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"), 
                                           legend_position = "bottom", bins = 70)
Mean_FB_NOR

combined_NOR <- Mean_UF_NOR %>% wrap_elements() | Mean_FB_NOR %>% wrap_elements()
ggsave(plot=combined_NOR, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_NOR_0.1.png",
       width = 8, height = 6)


Mean_UF_NOR_all <- function_spline_plot_groups(dataframe = combined, x_var = mean_vm5010_idx, group_var = Noradrenalin_increase_0.1_mcgkgmin, y_var = outcome_death_28d,
                                               x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                               show_overall = TRUE,
                                               #legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"),
                                               legend_position = "bottom", bins = 70)
Mean_UF_NOR_all
Mean_FB_NOR_all <- function_spline_plot_groups(dataframe = combined, x_var = mean_dm_balancerate_h, group_var = Noradrenalin_increase_0.1_mcgkgmin, y_var = outcome_death_28d,
                                               x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                               #legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"), 
                                               legend_position = "bottom", bins = 70)
Mean_FB_NOR_all

#############################################
## Subgroup plots: for Lactate increase

##CAVE: Filtered out patients with Increased Lactate at Start of CRRT

Lactate_df <- combined %>% filter(Lactate_increase_2 < 2, Lactate_increase_4 < 2)

Mean_UF_Lac_2 <- function_spline_plot_groups(dataframe = Lactate_df, x_var = mean_vm5010_idx, group_var = Lactate_increase_2, y_var = outcome_death_28d,
                                             x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                             show_overall = TRUE,
                                             legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"),
                                             legend_position = "bottom", bins = 70)
Mean_UF_Lac_2

Mean_UF_Lac_4 <- function_spline_plot_groups(dataframe = Lactate_df, x_var = mean_vm5010_idx, group_var = Lactate_increase_4, y_var = outcome_death_28d,
                                             x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                             show_overall = TRUE,
                                             legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"),
                                             legend_position = "bottom", bins = 70)
Mean_UF_Lac_4

Mean_FB_Lac_2 <- function_spline_plot_groups(dataframe = Lactate_df, x_var = mean_dm_balancerate_h, group_var = Lactate_increase_2, y_var = outcome_death_28d,
                                           x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                           legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"), 
                                           legend_position = "bottom", bins = 70)
Mean_FB_Lac_2

Mean_FB_Lac_4 <- function_spline_plot_groups(dataframe = Lactate_df, x_var = mean_dm_balancerate_h, group_var = Lactate_increase_4, y_var = outcome_death_28d,
                                             x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                             legend_labels = c("Overall", "No increase during CRRT", "Increase during CRRT"), 
                                             legend_position = "bottom", bins = 70)
Mean_FB_Lac_4

combined_Lac_2 <- Mean_UF_Lac_2 %>% wrap_elements() | Mean_FB_Lac_2 %>% wrap_elements()
ggsave(plot=combined_Lac_2, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_Lac_2.png",
       width = 8, height = 6)
combined_Lac_4 <- Mean_UF_Lac_4 %>% wrap_elements() | Mean_FB_Lac_4 %>% wrap_elements()
ggsave(plot=combined_Lac_4, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_Lac_4.png",
       width = 8, height = 6)

##############################################
## Subgroup plots: for Anuria increase

Anuria_df <- combined %>% filter(Anuria_onset < 2)

Mean_UF_Anuria <- function_spline_plot_groups(dataframe = Anuria_df, x_var = mean_vm5010_idx, group_var = Anuria_onset, y_var = outcome_death_28d,
                                             x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                             show_overall = TRUE,
                                             legend_labels = c("Overall", "Anuria during CRRT", "No new anuria onset during CRRT"),
                                             legend_position = "bottom", bins = 70)
Mean_UF_Anuria

Mean_FB_Anuria <- function_spline_plot_groups(dataframe = Anuria_df, x_var = mean_dm_balancerate_h, group_var = Anuria_onset, y_var = outcome_death_28d,
                                             x_label = "Mean Fluid Balance Change (ml/h)", y_label = "28-day mortality",
                                             legend_labels = c("Overall", "Anuria during CRRT", "No new anuria onset during CRRT"), 
                                             legend_position = "bottom", bins = 70)
Mean_FB_Anuria

combined_Anuria <- Mean_UF_Anuria %>% wrap_elements() | Mean_FB_Anuria %>% wrap_elements()
ggsave(plot=combined_Anuria, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_Anuria.png",
       width = 8, height = 6)

##############################################
## AUC plot for all subgroups

AUC_plot_database <- function_spline_plot_three_groups(dataframe = combined, x_var = UF_AUC, group_var = source, y_var = outcome_death_28d,
                                                       x_label = "Total Net Ultrafiltratoin Volume in 12 first Treatment Hours (ml/kg)",y_label = "28-day mortality",
                                                       legend_labels = c("Overall", "HiRID", "AmsterdamUMCdb"), 
                                                       legend_position = "bottom", bins = 70)
AUC_plot_database

AUC_plot_Sex <- function_spline_plot_three_groups(dataframe = combined, x_var = UF_AUC, group_var = gender, y_var = outcome_death_28d,
                                             x_label = "Total Net Ultrafiltratoin Volume in 12 first Treatment Hours (ml/kg)",y_label = "28-day mortality",
                                             legend_labels = c("Overall", "Female", "Male"), 
                                             legend_position = "bottom", bins = 70)
AUC_plot_Sex

AUC_plot_FO <- function_spline_plot_three_groups(dataframe = combined, x_var = UF_AUC, group_var = fluidoverload_10, y_var = outcome_death_28d,
                                                 x_label = "Total Net Ultrafiltratoin Volume in 12 first Treatment Hours (ml/kg)",y_label = "28-day mortality",
                                                 legend_labels = c("Overall", "Fluid overload ≥ 10%", "No Fluid overload ≥ 10%"), 
                                                 legend_position = "bottom", bins = 70)
AUC_plot_FO

AUC_combined <- AUC_plot_database | AUC_plot_Sex | AUC_plot_FO
ggsave(plot=AUC_combined, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_AUC.png", 
       width = 15, height = 6)

AUC_plot_NOR <- function_spline_plot_three_groups(dataframe = Noradre_df, x_var = UF_AUC, group_var = Noradrenalin_increase_0.1_mcgkgmin, y_var = outcome_death_28d,
                                                  x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                                  show_overall = TRUE,
                                                  legend_labels = c("Overall", "No Noradrenalin increase", "Noradrenalin increase > 0.1mcg/kg/min"),
                                                  legend_position = "bottom", bins = 70)
AUC_plot_NOR

AUC_plot_Lac2 <- function_spline_plot_three_groups(dataframe = Lactate_df, x_var = UF_AUC, group_var = Lactate_increase_2, y_var = outcome_death_28d,
                                                  x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                                  show_overall = TRUE,
                                                  legend_labels = c("Overall", "No Lactate increase", "Lactate Increase > 2mmol/l"),
                                                  legend_position = "bottom", bins = 70)
AUC_plot_Lac2

AUC_plot_Lac4 <- function_spline_plot_three_groups(dataframe = Lactate_df, x_var = UF_AUC, group_var = Lactate_increase_4, y_var = outcome_death_28d,
                                               x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                               show_overall = TRUE,
                                               legend_labels = c("Overall", "No Lactate increase", "Lactate Increase > 4mmol/l"),
                                               legend_position = "bottom", bins = 70)
AUC_plot_Lac4


AUC_combined_Lab <- AUC_plot_NOR | AUC_plot_Lac2 | AUC_plot_Lac4
ggsave(plot=AUC_combined_Lab, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_AUC_Labvalues.png", 
       width = 15, height = 6)

AUC_plot_anuria <- function_spline_plot_three_groups(dataframe = Anuria_df, x_var = UF_AUC, group_var = Anuria_onset, y_var = outcome_death_28d,
                                                     x_label = "Mean net Ultrafiltration Rate (ml/kg/h)", y_label = "28-day mortality",
                                                     show_overall = TRUE,
                                                     legend_labels = c("Overall", "Anuria during CRRT", "No new anuria onset during CRRT"),
                                                     legend_position = "bottom", bins = 70)
AUC_plot_anuria
ggsave(plot=AUC_plot_anuria, filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 2b\\eFigure 2b_AUC_Anuria.png", 
       width = 5, height = 6)
#################################################################################