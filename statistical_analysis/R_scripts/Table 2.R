#######################################
#######################################

library(tidyverse)
library(gt)
library(gtsummary)
library(flextable)
library(officer)
library(glue)

## Load data

stay_info <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\stays_fused_Total.csv")
regular <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_Total.csv")
changes <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\changes_UFperkg_Total.csv")
UF_and_FB <- read.csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Fluid_and_Ultrafiltration_Total.csv")

RRT_type <- read.csv("C:/Programming/sources/RRT_type.csv") %>% filter(Variable %in% c(2,3,4))

##################################################################################################
## Create a Table to describe Ultrafiltration ##

##Prepare the data for the table 
Session_length <- stay_info %>% mutate(session_length_d = session_length / 60 / 24) %>% dplyr::select(patid, session_length_d, source)
start_times <- changes %>% mutate(idx_h = idx * 5 / 60 / 24) %>%  # convert to days (idx is 5min timesteps)
  group_by(patid) %>%  summarise(start_of_CRRT_d = first(idx_h[timepoint_label == "start"]),
                                 start_of_UF_d = first(idx_h[vm5010_idx > 0]),
                                 .groups = "drop") %>% 
  mutate(time_until_UF_h = (start_of_UF_d - start_of_CRRT_d)*24)
time_UF <- regular %>% filter(NaN_converted_to_0 == 0) %>% group_by(patid) %>%  
  mutate(UF_category = ifelse(vm5010_idx == 0, "UF Rate 0", "UF Rate > 0")) %>% 
  summarise(percentage_UF_nonnull = sum(UF_category == "UF Rate > 0", na.rm = TRUE) / n() * 100,
            percentage_UF_0 = sum(UF_category == "UF Rate 0", na.rm = TRUE) / n() * 100,
            .groups = "drop") %>% 
  mutate(only_UF_0 = ifelse(percentage_UF_0 == 100, 1, 0))

RRT_types_changes <- changes %>% group_by(patid) %>% reframe(RRT_type = first(vm5025, na_rm = TRUE))
RRT_types_regular <- regular %>% group_by(patid) %>% reframe(RRT_type = first(vm5025, na_rm = TRUE))
RRT_types_total <- full_join(RRT_types_changes, RRT_types_regular, by="patid") %>%
  mutate(RRT_type_final = coalesce(RRT_type.x, RRT_type.y)) %>% 
  dplyr::select(patid, RRT_type_final)

UF <- UF_and_FB %>% dplyr::select(patid, mean_vm5010_idx, median_vm5010_idx, mean_UFnet_bin)
UF <- left_join(UF, RRT_types_total, by="patid")

data_tabletwo_a <- inner_join(Session_length, start_times, by="patid")
data_tabletwo_a <- inner_join(data_tabletwo_a, time_UF, by="patid")
data_tabletwo_a <- inner_join(data_tabletwo_a, UF, by="patid")

data_tabletwo_a$source <- factor(data_tabletwo_a$source, levels = c("AmsterdamUMCDb", "HiRID"), labels = c("Amsterdam UMC", "HiRID"))
data_tabletwo_a$RRT_type_final <- factor(data_tabletwo_a$RRT_type_final, levels = RRT_type$Variable, labels = RRT_type$Name)
data_tabletwo_a$mean_UFnet_bin <- factor(data_tabletwo_a$mean_UFnet_bin, levels = c("zero", "low (<1.01)", "moderate (1.01-1.75)", "high (>1.75)"), labels = c("Zero (0)", "Low (0.01 - 1.0)", "Moderate (1.01-1.75)", "High (>1.75)"))

labelstolabel2a <- list(
  RRT_type_final = "Type of Continuous Renal Replacement Therapy",
  session_length_d = "Duration of Continuous Renal Replacement Therapy (days)",
  start_of_CRRT_d = "Duration until Start of Continuous Renal Replacement Therapy from Intensive Care Unit Admission (days)",
  time_until_UF_h = "Duration from Start of Continuous Renal Replacement Therapy until Ultrafiltration (hours)",
  percentage_UF_0 = "Percentage of Time with Ultrafiltration = 0",
  only_UF_0 = "Patients without Ultrafiltration > 0 during whole Session of Continuous Renal Replacement Therapy",
  mean_vm5010_idx = "Mean Ultrafiltration rate (ml/kg/h)",
  mean_UFnet_bin = "Group of Mean Ultrafiltration Rate (ml/kg/h)"
)

Table2a <- data_tabletwo_a %>% dplyr::select(source, 
                                             RRT_type_final, 
                                             session_length_d, start_of_CRRT_d,
                                             time_until_UF_h,
                                             mean_vm5010_idx, mean_UFnet_bin,
                                             percentage_UF_0, only_UF_0
) %>% 
  tbl_summary(by = source, 
              label = labelstolabel2a, missing = "no",
              statistic = list(session_length_d ~ "{median} ({p25}-{p75})",
                               start_of_CRRT_d ~ "{median} ({p25}-{p75})",
                               time_until_UF_h ~ "{median} ({p25}-{p75})",
                               mean_vm5010_idx ~ "{mean} ({sd})",
                               percentage_UF_0 ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"
              )) %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Database**") %>%
  add_overall()

Table2a

##################################################################################################
## Create a table to describe change of Fluidbalance ##

## Prepare the data for the table 
removed_fluid <- regular %>% group_by(patid) %>% 
  summarise(cumulative_fluid_change_CRRT_L = sum(dm_balancerate_h, na.rm = TRUE) / 1000, 
            fluid_balance_pre_crrt_L = mean(fluid_balance_pre_crrt) / 1000,
            .groups = "drop")
removed_fluid_by_UF_L <- changes %>% group_by(patid) %>% 
  mutate(duration_h = (lead(session_length) - session_length)/60) %>% 
  mutate(fluid_ml = duration_h * vm5010) %>% dplyr::select(patid, session_length, duration_h, vm5010, vm5010_idx, fluid_ml) %>% 
  summarise(UF_fluid_L = -sum(fluid_ml, na.rm = TRUE)/1000, .groups = "drop") 
removed_fluid <- inner_join(removed_fluid, removed_fluid_by_UF_L, by="patid") 

stay_info_FB <- stay_info
Fluid <- left_join(stay_info_FB %>% mutate(session_length_d = session_length / 60 / 24, 
                                           fluidoverload_category = ifelse(fluidoverload <0, "negative FB", "positive FB")) %>% 
                     dplyr::select(patid, fluidoverload, fluidoverload_category, source, session_length_d), 
                   UF_and_FB %>% dplyr::select(patid, mean_dm_balancerate_h, median_dm_balancerate_h, mean_FB_bin), by="patid")
Fluid <- inner_join(removed_fluid, Fluid, by="patid") %>% 
  mutate(FB_at_end_of_CRRT = fluid_balance_pre_crrt_L + cumulative_fluid_change_CRRT_L,
         FB_category_at_end_CRRT = ifelse(FB_at_end_of_CRRT < 0, "negative FB", "positive FB"), 
         FB_category = ifelse(cumulative_fluid_change_CRRT_L < 0, "negative FB change", "positive FB change")) %>% 
  mutate(UF_fluid_L_d = UF_fluid_L / session_length_d,
         cumulative_fluid_change_CRRT_L_d = cumulative_fluid_change_CRRT_L / session_length_d)

Percent_Fluidoverload_Change <- Fluid
Percent_Fluidoverload_Change <- Percent_Fluidoverload_Change %>% 
  #Calculate the Percentage change only for patients with positive Fluid balance pre_CRRT
  filter(fluidoverload_category == "positive FB") %>% 
  mutate(percent_change_fluid_balance = (cumulative_fluid_change_CRRT_L / fluidoverload) * 100) %>% 
  dplyr::select(patid, percent_change_fluid_balance) 

Fluid <- left_join(Fluid, Percent_Fluidoverload_Change, by="patid")

time_FB <- regular %>% group_by(patid) %>%  
  mutate(FB_category = ifelse(dm_balancerate_h < 0, "FB change neg", "FB change pos"),
         UF_category = ifelse(vm5010_idx == 0, "UF Rate 0", "UF Rate > 0")) %>% 
  summarise(percentage_FB_neg = sum(FB_category == "FB change neg", na.rm = TRUE) / n() * 100,
            percentage_FB_pos = sum(FB_category == "FB change pos", na.rm = TRUE) / n() * 100,
            percentage_FB_neg_during_UF = sum(FB_category == "FB change neg" & UF_category == "UF Rate > 0", na.rm = TRUE) / 
              sum(UF_category == "UF Rate > 0", na.rm = TRUE) * 100,
            .groups = "drop") %>% 
  mutate(only_pos_FB = ifelse(percentage_FB_neg == 0,1,0),
         only_pos_FB_during_UF = ifelse(percentage_FB_neg_during_UF == 0, 1, 0),
         only_neg_FB_during_UF = ifelse(percentage_FB_neg_during_UF == 100, 1, 0))

data_tabletwo_b <- inner_join(Fluid, time_FB, by="patid")
data_tabletwo_b$source <- factor(data_tabletwo_b$source, levels = c("AmsterdamUMCDb", "HiRID"), labels = c("Amsterdam UMC", "HiRID"))
data_tabletwo_b$FB_category <- factor(data_tabletwo_b$FB_category, levels = c("negative FB change", "positive FB change"), labels = c("Negative Fluid Change", "Positive Fluid Change"))
data_tabletwo_b$FB_category_at_end_CRRT <- factor(data_tabletwo_b$FB_category_at_end_CRRT, levels = c("negative FB", "positive FB"), labels = c("Negative Fluid Balance","Positive Fluid Balance"))
data_tabletwo_b$mean_FB_bin <- factor(data_tabletwo_b$mean_FB_bin, 
                                      levels = c("positive (>20.833)","neutral (-20.833 - 20.833)", "light_negative (-62.5 - -20.833)","strong_negative (< -62.5)"), 
                                      labels = c("Positive (>20.83)","Neutral (-20.83 - 20.83)", "Light Negative (-62.5 - -20.84)","Strong Negative (< -62.5)"))

labelstolabel2b <- list(
  mean_dm_balancerate_h = "Mean hourly Change of Fluid Balance (ml/h)",
  mean_FB_bin = "Group of Mean Fluid Balance Change (ml/h)",
  FB_at_end_of_CRRT = "Cumulative Fluid Balance at the End of Continuous Renal Replacement Therapy (L)",
  FB_category_at_end_CRRT = "Cumulative Fluid Balance at the End of Continuous Renal Replacement Therapy:",
  FB_category = "Total Fluid Change during Continuous Renal Replacement Therapy:",
  cumulative_fluid_change_CRRT_L = "Total Fluid Change during Continuous Renal Replacement Therapy (L)",
  cumulative_fluid_change_CRRT_L_d = "Total Fluid Change during Continuous Renal Replacement Therapy per day (L/day)",
  UF_fluid_L = "Total Fluid removed by Continuous Renal Replacement Therapy (L)",
  UF_fluid_L_d = "Total Fluid removed by Continuous Renal Replacement Therapy per day (L/day)",
  percent_change_fluid_balance = "Relative Fluid Balance Change (% of Fluid Balance before Continuous Renal Replacement Therapy) in Patients with positive Fluid Balance",
  percentage_FB_neg = "Percentage of Time with negative hourly Fluid Balance Change during Continuous Renal Replacement Therapy (%)",
  percentage_FB_neg_during_UF = "Percentage of Time with negative hourly Fluid Balance Change when Ultrafiltration > 0 (%)"
)

Table2b <- data_tabletwo_b %>% dplyr::select(source, 
                                             mean_dm_balancerate_h,mean_FB_bin,
                                             FB_at_end_of_CRRT, FB_category_at_end_CRRT, cumulative_fluid_change_CRRT_L, FB_category, 
                                             cumulative_fluid_change_CRRT_L_d, UF_fluid_L, UF_fluid_L_d, percent_change_fluid_balance,
                                             percentage_FB_neg, percentage_FB_neg_during_UF
) %>% 
  tbl_summary(by = source, label = labelstolabel2b, missing = "no",
              statistic = list(mean_dm_balancerate_h  ~ "{mean} ({sd})", 
                               FB_at_end_of_CRRT ~ "{median} ({p25}-{p75})",
                               cumulative_fluid_change_CRRT_L ~ "{median} ({p25}-{p75})", 
                               cumulative_fluid_change_CRRT_L_d ~ "{median} ({p25}-{p75})",
                               percent_change_fluid_balance ~ "{median} ({p25}-{p75})",
                               UF_fluid_L ~ "{median} ({p25}-{p75})",
                               UF_fluid_L_d  ~ "{median} ({p25}-{p75})",
                               percentage_FB_neg ~ "{mean} ({sd})",
                               percentage_FB_neg_during_UF ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"
              )) %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Database**") %>%
  add_overall() 

Table2b

##################################################################################################

##Combine both tables

Table2 <- tbl_stack(list(Table2a, Table2b))
Table2

#Save as word
Table2_gt <- as_gt(Table2) %>%
  tab_footnote(footnote = "Negative < 0", locations = cells_body(
    columns = label,rows = label %in% c("Negative Fluid Balance", "Negative Fluid Change"))) %>% 
  tab_footnote(footnote= "Positive >= 0",locations = cells_body(
      columns = label,rows = label %in% c("Positive Fluid Balance", "Positive Fluid Change")))
gtsave(Table2_gt, filename= "Table_2.docx", path = "C:\\Programming\\FLIRRT\\FLIRRT_Paper\\")
