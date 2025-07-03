##Partial dependence plots using GAM or tree based models
library(dplyr)
library(readr)
library(pdp)
library(randomForest)
library(patchwork)
library(ggplot2)
library(plotly)

## Load data
UF_and_FB <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Fluid_and_Ultrafiltration_Total.csv")
covariates <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\stays_fused_Total.csv")
combined <- inner_join(UF_and_FB, covariates, by="patid")
combined <- combined %>% filter(!is.na(apache_score), !is.na(invasive_ventilation))

################################################################################

# 1.1 1D PDP with actual predicted probability of death! Not relative :)
## For Fluid balance (different summary statistics, rf model with UF mean)

# Fit RF model
rf_model <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx + mean_dm_balancerate_h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rf_model_fbq1 <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx + Q1_dm_balancerate_h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rf_model_fb48 <-  randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx_48h + mean_dm_balancerate_h_48h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rd_model_fbneg <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx + mean_dm_balancerate_h_FBneg + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined %>% dplyr::filter(!is.na(mean_dm_balancerate_h_FBneg)))
pdp_fb <- partial(rf_model, 
                  pred.var = "mean_dm_balancerate_h", 
                  prob = TRUE, which.class = 2,
                  train = combined)  # 2 = "1" = death
pdp_fb_q1 <- partial(rf_model_fbq1,
                     pred.var = "Q1_dm_balancerate_h", 
                     prob = TRUE, which.class = 2,
                     train = combined)
pdp_fb_48 <- partial(rf_model_fb48,
                     pred.var = "mean_dm_balancerate_h_48h", 
                     prob = TRUE, which.class = 2,
                     train = combined)
pdp_fb_neg <- partial(rd_model_fbneg,
                      pred.var = "mean_dm_balancerate_h_FBneg", 
                      prob = TRUE, which.class = 2,
                      train = combined)
p_fb <- autoplot(pdp_fb,
                 xlab = "Mean Change of Fluid Balance (ml/h)",
                 ylab = "Predicted probability of 28-day death") + coord_cartesian(xlim = c(-1000,600), ylim = c(0, 1))
p_fb_q1 <- autoplot(pdp_fb_q1,
                    xlab = "75th Percentile of Change of Fluid Balance (ml/h)",
                    ylab = "Predicted probability of 28-day death")  + coord_cartesian(xlim = c(-1000,600), ylim = c(0, 1))
p_fb_48 <- autoplot(pdp_fb_48,
                    xlab = "Mean Change of Fluid Balance < 48h (ml/h)",
                    ylab = "Predicted probability of 28-day death")  + coord_cartesian(xlim = c(-1000,600), ylim = c(0, 1))
p_fb_neg <- autoplot(pdp_fb_neg,
                     xlab = "Mean Change of Fluid Balance when < 0 (ml/h)",
                     ylab = "Predicted probability of 28-day death")  + coord_cartesian(xlim = c(-1000,600), ylim = c(0, 1))
combined_fb_partial <- p_fb / p_fb_q1/ p_fb_48 / p_fb_neg
ggsave(filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 6b.png",
       plot = combined_fb_partial, width = 8, height = 12, dpi = 300)

## For Ultrafiltration Rate (different summary statistics, rf model with mean FB change)
rf_model <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx + mean_dm_balancerate_h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rf_model_ufQ3 <- randomForest(as.factor(outcome_death_28d) ~ Q3_vm5010_idx + mean_dm_balancerate_h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rf_model_UF48 <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx_48h + mean_dm_balancerate_h_48h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined)
rf_model_UFpos <- randomForest(as.factor(outcome_death_28d) ~ mean_vm5010_idx_UFpos + mean_dm_balancerate_h + age_at_admission + gender + BMI + apache_score + SOFA_score + emergency_admission + invasive_ventilation, data = combined %>% dplyr::filter(!is.na(mean_vm5010_idx_UFpos)))

pdp_uf <- partial(rf_model, 
                  pred.var = "mean_vm5010_idx", 
                  prob = TRUE, which.class = 2,
                  train = combined)  # 2 = "1" = death
pdp_uf_q3 <- partial(rf_model_ufQ3, 
                  pred.var = "Q3_vm5010_idx", 
                  prob = TRUE, which.class = 2,
                  train = combined)  # 2 = "1" = death
pdp_uf_48 <- partial(rf_model_UF48, 
                  pred.var = "mean_vm5010_idx_48h", 
                  prob = TRUE, which.class = 2,
                  train = combined)  # 2 = "1" = death
pdp_uf_pos <- partial(rf_model_UFpos, 
                  pred.var = "mean_vm5010_idx_UFpos", 
                  prob = TRUE, which.class = 2,
                  train = combined)  # 2 = "1" = death

p_uf <- autoplot(pdp_uf,
                 xlab = "Mean net Ultrafiltration Rate (ml/kg/h)",
                 ylab = "Predicted probability of 28-day death")+ coord_cartesian(xlim = c(0,5), ylim = c(0, 1))
p_uf_q3 <- autoplot(pdp_uf_q3,
                 xlab = "75th Percentile net Ultrafiltration Rate (ml/kg/h)",
                 ylab = "Predicted probability of 28-day death")+ coord_cartesian(xlim = c(0,5), ylim = c(0, 1))
p_uf_48 <- autoplot(pdp_uf_48,
                    xlab = "Mean net Ultrafiltration Rate < 48h (ml/kg/h)",
                    ylab = "Predicted probability of 28-day death")+ coord_cartesian(xlim = c(0,5), ylim = c(0, 1))
p_uf_pos <- autoplot(pdp_uf_pos,
                     xlab = "Mean net Ultrafiltration Rate when > 0 (ml/kg/h)",
                     ylab = "Predicted probability of 28-day death") + coord_cartesian(xlim = c(0,5), ylim = c(0, 1))
# Save the plot using ggsave
ggsave(filename = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 6a.png",
       plot = (p_uf / p_uf_q3/ p_uf_48 / p_uf_pos), width = 8, height = 12, dpi = 300)


# 2D PDP
pdp_2d <- partial(rf_model, pred.var = c("mean_vm5010_idx", "mean_dm_balancerate_h"),
                  prob = TRUE, which.class = 2)  # 2 = "1" = death)
plotPartial(pdp_2d, levelplot = TRUE, 
            xlab = "Mean UF rate (ml/kg/h)",
            ylab = "Mean fluid balance rate (ml/h)")

# Plot with ggplot2 and if wanted overlay data points
ggplot(pdp_2d, aes(x = mean_vm5010_idx, y = mean_dm_balancerate_h)) +
  geom_tile(aes(fill = yhat)) +  # PDP values as heatmap
  #geom_point(data = combined, aes(x = mean_vm5010_idx, y = mean_dm_balancerate_h), color = "white", alpha = 0.1, shape = 16, size = 1.5) +  # actual data points
  scale_fill_viridis_c(option = "A", name = "Mortality\nprobability") +
  labs(x = "Mean UF rate (ml/kg/h)",
       y = "Mean fluid balance rate (ml/h)") +
  theme_minimal()
ggsave(filename="C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 6c_2D Partial Dependence Plot.png",
       width = 6, height = 6, bg="white")


######################
# Create a 3D Plot of predicted probability of 28-day mortality, when keeping everything constant besides NUF and FB change :)

# Rename columns for clarity
pdp_2d_copy <- pdp_2d
colnames(pdp_2d_copy) <- c("UF", "FB", "prob")
# Create a grid for the surface
surface_data <- reshape2::acast(pdp_2d_copy, FB ~ UF, value.var = "prob")

# Plot
pdp_3D <- plot_ly(x = sort(unique(pdp_2d_copy$UF)),
                  y = sort(unique(pdp_2d_copy$FB)),
                  z = surface_data) %>%
            add_surface() %>%
            layout(scene = list(
              xaxis = list(title = "Mean net Ultrafiltration rate (ml/kg/h)"),
              yaxis = list(title = "Mean Fluid Balance Change (ml/h)"),
              zaxis = list(title = "Predicted 28-day mortality")
            ))
htmlwidgets::saveWidget(pdp_3D, "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\eFigure 6d_3D_PDP_plot.html")
