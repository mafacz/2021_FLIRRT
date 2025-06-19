library(readr)
library(dplyr)
library(ggplot2)

######################################################
### invasive/mechanische Beatmung zum Zeitpunkt -1 ###
######################################################
population <- read_csv("C:/data/processed/2021_FLIRRT/stays.csv")
regular <- read_csv("C:/data/processed/2021_FLIRRT/regular.csv")

## lade alle werte vor beginn crrt die dich interessieren:
pre_crrt_values <- regular %>% filter(timepoint_label == -1) %>% select(patid, dm_vent_inv_state)  #hier kannst du alle weiteren werte erg?nzen die dich interessieren

# load pre crrt values into population table
pop_augmented <- left_join(population, pre_crrt_values, by = c("patientid" = "patid"))

## Calculate BMI
pop_augmented$BMI <- (pop_augmented$weight_at_admission / (pop_augmented$height_at_admission/100)^2)

## Weight/Height outlier detection and correction
pop_augmented$BMI_outl <- pop_augmented$BMI > 65 | pop_augmented$BMI < 10

ggplot(pop_augmented, aes(x = height_at_admission, y = weight_at_admission)) +
  geom_point(aes(colour = BMI_outl))

## spalten umbenennen (length statt lenght)
pop_augmented <- pop_augmented %>% rename("length_of_stay" = "lenght_of_stay") 
pop_augmented <- pop_augmented %>% rename("session_length" = "session_lenght")

################################################
#### missing values aus weight_at_admission ####
################################################

## Patient gem?ss Einschlusskriterien filtern. Ich w?rde zwischenschritte hier in separaten variablen speichern um danach resultate ausgeben zu k?nnen
pop_augmented_withweight <- pop_augmented[!is.na(pop_augmented$weight_at_admission),]
pop_augmented_stayduration <- pop_augmented_withweight[pop_augmented_withweight$length_of_stay >= 86400,]  #lenght of stay > 24h
pop_augmented_bmi <- pop_augmented_stayduration[pop_augmented_stayduration$BMI > 10 & pop_augmented_stayduration$BMI < 65,]

############################
##### Filter resultate #####
############################
sprintf("Total pop with CRRT: %s", nrow(pop_augmented))
sprintf("Excluded with missing weight: %s", nrow(pop_augmented)-nrow(pop_augmented_withweight))
sprintf("Excluded with short stay: %s", nrow(pop_augmented_withweight)-nrow(pop_augmented_stayduration))
sprintf("Excluded with out of range BMI: %s", nrow(pop_augmented_stayduration)-nrow(pop_augmented_bmi))
sprintf("Total included: %s", nrow(pop_augmented_bmi))

#resultate speichern
write_csv(pop_augmented_bmi, path = "C:/data/processed/2021_FLIRRT/pop_included.csv")
