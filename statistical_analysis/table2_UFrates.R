library(readr)
library(dplyr)
library(table1)
library(ggplot2)
library(gridExtra)
library(grid)
library(knitr)

##################################
##### Load data #####
##################################

#######CAVE!!!!!!!!!!!!!there seems to be a mistake in the changes csv. the one who have only a start and an end do not have any entries

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
##### Descriptive statistics UF Rate #####
##################################

####Histogram of absolute and indexed UF rate
# Calculate mean and median for UF_absolute
mean_UF_absolute <- mean(measurements_changes_augmented$vm5010, na.rm = TRUE)
median_UF_absolute <- median(measurements_changes_augmented$vm5010, na.rm = TRUE)

# Calculate mean and median for UF_indexed
mean_UF_indexed <- mean(measurements_changes_augmented$UF_indexed, na.rm = TRUE)
median_UF_indexed <- median(measurements_changes_augmented$UF_indexed, na.rm = TRUE)

# Plot for UF_absolute (vm5010)
p1 <- ggplot(measurements_changes_augmented, aes(x = vm5010)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = 'blue', binwidth = 10) +
  geom_vline(xintercept = mean_UF_absolute, color = "blue", linetype = "dashed", size = 1) +
  geom_text(aes(x = mean_UF_absolute + 30, label = "Mean"), y = 0, vjust = -1.5, color = "black") +
  labs(title = "UF_absolute distribution",
       x = "UF Absolute",
       y = "Density")

# Plot for UF_indexed
p2 <- ggplot(measurements_changes_augmented, aes(x = UF_indexed)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, fill = 'blue', binwidth = 0.2) +
  geom_vline(xintercept = mean_UF_indexed, color = "blue", linetype = "dashed", size = 1) +
  geom_text(aes(x = mean_UF_indexed + 0.5, label = "Mean"), y = 0, vjust = -1.5, color = "black") +
  labs(title = "UF_indexed distribution",
       x = "UF Indexed",
       y = "Density")

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)


###numeric descriptive variables
# Select the variables
selected_vars <- measurements_changes_augmented %>%
  select(UF_absolute = vm5010, UF_indexed)

# Calculate descriptive statistics UF Rate
desc_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "IQR"),
  UF_absolute = c(
    mean(selected_vars$UF_absolute, na.rm = TRUE),
    median(selected_vars$UF_absolute, na.rm = TRUE),
    sd(selected_vars$UF_absolute, na.rm = TRUE),
    min(selected_vars$UF_absolute, na.rm = TRUE),
    max(selected_vars$UF_absolute, na.rm = TRUE),
    IQR(selected_vars$UF_absolute, na.rm = TRUE) # Adding IQR
  ),
  UF_indexed = c(
    mean(selected_vars$UF_indexed, na.rm = TRUE),
    median(selected_vars$UF_indexed, na.rm = TRUE),
    sd(selected_vars$UF_indexed, na.rm = TRUE),
    min(selected_vars$UF_indexed, na.rm = TRUE),
    max(selected_vars$UF_indexed, na.rm = TRUE),
    IQR(selected_vars$UF_indexed, na.rm = TRUE) # Adding IQR
  )
)

##################################
##### Descriptive statistics for change frequency #####
##################################

#calculate change times
measurements_changes_augmented <- measurements_changes_augmented %>%
  group_by(patid) %>%
  mutate(duration_between_UF_changes = (session_lenght - lag(session_lenght))/60) %>%
  ungroup()

#generate histogram
x_limits <- quantile(measurements_changes_augmented$duration_between_UF_changes, probs = c(0.0, 0.95), na.rm = TRUE)
ggplot(measurements_changes_augmented, aes(x = duration_between_UF_changes)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.5) +
  labs(title = "Distribution of Durations Between UF Changes",
       x = "Duration [h]",
       y = "Count") +
  xlim(x_limits)

selected_vars <- measurements_changes_augmented %>%
  select(duration_between_UF_changes)

# Calculate descriptive statistics UF Rate
change_freq_desc_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "IQR"),
  Duration_between_UFchanges = c(
    mean(selected_vars$duration_between_UF_changes, na.rm = TRUE),
    median(selected_vars$duration_between_UF_changes, na.rm = TRUE),
    sd(selected_vars$duration_between_UF_changes, na.rm = TRUE),
    min(selected_vars$duration_between_UF_changes, na.rm = TRUE),
    max(selected_vars$duration_between_UF_changes, na.rm = TRUE),
    IQR(selected_vars$duration_between_UF_changes, na.rm = TRUE) # Adding IQR
  )
)

#####################################
##### Descriptive statistics on how frequently positive UF #####
##################################

measurements_changes_augmented <- measurements_changes_augmented %>%
  mutate(UF_category = ifelse(vm5010 == 0, "UF Rate 0", "UF Rate > 0"))

# Calculate Duration
measurements_changes_augmented <- measurements_changes_augmented %>%
  group_by(patid) %>%
  mutate(duration = (session_lenght - lag(session_lenght))/60) %>% # Adjust units as needed
  ungroup()

# Calculate Percentage for Each Patient
percentage_time_UF_0 <- measurements_changes_augmented %>%
  group_by(patid) %>%
  summarise(
    total_time = sum(duration, na.rm = TRUE),
    time_UF_0 = sum(ifelse(UF_category == "UF Rate 0", duration, 0), na.rm = TRUE),
    percentage_UF_0 = time_UF_0 / total_time * 100
  )

desc_stats_UF_0 <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "IQR"),
  Value = c(
    mean(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE),
    median(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE),
    sd(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE),
    min(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE),
    max(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE),
    IQR(percentage_time_UF_0$percentage_UF_0, na.rm = TRUE)
  )
)

hist(percentage_time_UF_0$percentage_UF_0)


###########################################
##### Oveall Output
###########################################
# Combine all four data frames by columns
final_desc_stats <- cbind(desc_stats, change_freq_desc_stats[,2], desc_stats_UF_0[,2])

colnames(final_desc_stats) <- c("", "UF_absolute distribution", "UF_indexed distribution", "Durations Between UF Changes [h]", "% of UF=0")

#print
result_table <- tableGrob(t(final_desc_stats))
grid.newpage()
grid.draw(result_table)

