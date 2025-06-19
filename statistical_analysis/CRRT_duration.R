library(readr)
library(dplyr)
library(table1)
library(ggplot2)
library(gridExtra)
library(grid)
library(knitr)
library(survival)
library(survminer)
library(caret)
library(ggplot2)

##################################
##### Load data #####
##################################

Pop <- read_csv("C:/data/processed/2021_FLIRRT/pop_included.csv")
measurements_changes <- read_csv("C:/data/processed/2021_FLIRRT/changes.csv")

##################################
##### Filter the population and augment data #####
##################################
population_values <- Pop %>% select(patientid, weight_at_admission)

# load pre crrt values into population table
# at the same time FILTER FOR INCLUDED POPULATION
measurements_changes_augmented <- inner_join(measurements_changes, population_values, by = c("patid" = "patientid"))
measurements_changes_augmented <- measurements_changes_augmented %>% arrange(patid, session_lenght)

##################################
##### Indexing by weight #####
##################################
measurements_changes_augmented$UF_indexed = measurements_changes_augmented$vm5010/measurements_changes_augmented$weight_at_admission

##################################
##### ANOVA  #####
##################################
measurements_changes_augmented <- measurements_changes_augmented %>%
  group_by(patid) %>%
  summarise(average_UF_indexed = mean(UF_indexed, na.rm = TRUE), 
            session_lenght = max(session_lenght))

measurements_changes_augmented <- measurements_changes_augmented %>%   #split according to the current literature
  mutate(UF_group = case_when(
    average_UF_indexed <= 1.01 ~ "Low",
    average_UF_indexed > 1.01 & average_UF_indexed <= 1.75 ~ "Medium",
    average_UF_indexed > 1.75 ~ "High"
  ))
measurements_changes_augmented$UF_group <- as.factor(measurements_changes_augmented$UF_group)

##################################
##### Filter the population and augment data #####
##################################
measurements_changes_augmented$session_lenght_h <- measurements_changes_augmented$session_lenght / 60

#create a survival object using the Surv function
surv_obj <- Surv(measurements_changes_augmented$session_lenght_h, rep(1, length(measurements_changes_augmented$session_lenght)))

# Now you can fit a survival model using the survfit function
surv_model <- survfit(surv_obj ~ UF_group, data = measurements_changes_augmented)  # "~ 1" means no stratification

# Plot the Kaplan-Meier curve using the ggsurvplot function from the survminer package
ggsurvplot(surv_model, title="Duration of CRRT")




