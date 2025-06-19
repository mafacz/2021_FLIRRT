library(readr)
library(dplyr)
library(ggplot2)

##################################
##### Load data #####
##################################

#######CAVE!!!!!!!!!!!!!there seems to be a mistake in the changes csv. the one who have only a start and an end do not have any entries

Pop <- read_csv("C:/data/processed/2021_FLIRRT/pop_included.csv")
measurements_changes <- read_csv("C:/data/processed/2021_FLIRRT/changes.csv")

##################################
##### Filter the population and augment data #####
##################################
population_values <- Pop %>% select(patientid, weight_at_admission, outcome_death_30d, age_at_admission, emergency_admission, adm_apache_group, apache_score) %>% filter(outcome_death_30d >= 0)

# load pre crrt values into population table
# at the same time FILTER FOR INCLUDED POPULATION
measurements_changes_augmented <- inner_join(measurements_changes, population_values, by = c("patid" = "patientid"))
measurements_changes_augmented <- measurements_changes_augmented %>% arrange(patid, session_lenght)

##################################
##### Indexing by weight #####
##################################
measurements_changes_augmented$UF_indexed = measurements_changes_augmented$vm5010/measurements_changes_augmented$weight_at_admission


##################################
##### Correlation for UF Rate #####
##################################
averaged_data <- measurements_changes_augmented %>%
  group_by(patid) %>%
  summarise(average_vm5010 = mean(vm5010, na.rm = TRUE), 
            outcome_death_30d = first(outcome_death_30d))

correlation <- cor.test(averaged_data$average_vm5010, averaged_data$outcome_death_30d, method = "spearman")
print(correlation)

ggplot(averaged_data, aes(x = average_vm5010, y = outcome_death_30d)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(title = "Relationship between Average Ultrafiltration Rate and 30-day Mortality",
       x = "Average Ultrafiltration Rate (vm5010)",
       y = "30-day Mortality (outcome_death_30d)")


##################################
##### Correlation for UF Rate Indexed #####
##################################
averaged_data <- measurements_changes_augmented %>%
  group_by(patid) %>%
  summarise(average_UF_indexed = mean(UF_indexed, na.rm = TRUE), 
            outcome_death_30d = first(outcome_death_30d))

correlation <- cor.test(averaged_data$average_UF_indexed, averaged_data$outcome_death_30d, method = "spearman")
print(correlation)

ggplot(averaged_data, aes(x = average_UF_indexed, y = outcome_death_30d)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_vline(xintercept = 1.01, linetype="dashed", color = "red") +
  geom_vline(xintercept = 1.75, linetype="dashed", color = "red") +
  labs(title = "Relationship between Average Indexed Ultrafiltration Rate and 30-day Mortality",
       x = "Average Indexed Ultrafiltration Rate (average_UF_indexed)",
       y = "30-day Mortality (outcome_death_30d)")

ggplot(averaged_data, aes(x=factor(outcome_death_30d), y=average_UF_indexed)) + 
  geom_boxplot() +
  labs(x="30-Day Outcome", y="Average UF Indexed") 


##################################
##### ANOVA  #####
##################################
averaged_data <- measurements_changes_augmented %>%
  group_by(patid) %>%
      summarise(average_UF_indexed = mean(UF_indexed, na.rm = TRUE), 
                outcome_death_30d = first(outcome_death_30d),
                age_at_admission = first(age_at_admission),
                emergency_admission = first(emergency_admission),
                adm_apache_group = as.factor(first(adm_apache_group)),
                apache_score = first(apache_score))

averaged_data <- averaged_data %>%   #split according to the current literature
  mutate(UF_group = case_when(
    average_UF_indexed <= 1.01 ~ "Low",
    average_UF_indexed > 1.01 & average_UF_indexed <= 1.75 ~ "Medium",
    average_UF_indexed > 1.75 ~ "High"
  ))


####Univariate ANOVA
anova_result <- aov(outcome_death_30d ~ UF_group, data = averaged_data)
summary(anova_result)
posthoc <- TukeyHSD(anova_result)
print(posthoc)


####Adjusted ANOVA
anova_result <- aov(outcome_death_30d ~ UF_group + age_at_admission + emergency_admission + adm_apache_group + apache_score, data = averaged_data)
summary(anova_result)

