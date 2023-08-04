library(readr)
library(dplyr)
library(table1)

##################################
##### TABLE 1 #####
##################################
Pop1 <- read_csv("C:/data/processed/2021_FLIRRT/pop_included.csv")
apache_gr <- read_csv("C:/data/processed/2021_FLIRRT/apache_gr.csv")

Pop1$length_of_stay <- Pop1$length_of_stay/86400          ## Umwandlung in TAGE

Pop1$outcome_death_30d <-
  factor(Pop1$outcome_death_30d,
         levels = c(0, 1, -1),
         labels =c("Alive", "Dead", "Unknown"))
Pop1$outcome_icu_death <-
  factor(Pop1$outcome_icu_death,
         levels = c(TRUE, FALSE),
         labels = c("Non-Survivor", "Survivor"))
Pop1$gender <-
  factor(Pop1$gender,
         levels = c("F", "M"),
         labels = c("Female", "Male"))
Pop1$emergency_admission <-
  factor(Pop1$emergency_admission,
         levels = c(TRUE, FALSE),
         labels = c("Yes", "No"))
Pop1$dm_vent_inv_state <-
  factor(Pop1$dm_vent_inv_state,
         levels = c(TRUE, FALSE),
         labels = c("Yes", "No"))
Pop1$adm_apache_group <-
  factor(Pop1$adm_apache_group,
         levels = c(apache_gr$metavariable),
         labels = c(apache_gr$groupname))

labels <- list(
  variables = list(gender = "Gender",
                   age_at_admission = "Age (years)",
                   height_at_admission = "Height (cm)",
                   weight_at_admission = "Weight (kg)",
                   BMI = "Body Mass Index (kg/m^2)",
                   emergency_admission = "Emergency admission",
                   adm_apache_group = "Reason for ICU admission",
                   apache_score = "APACHE II Score",
                   dm_vent_inv_state = "Mechanical ventilation",
                   length_of_stay = "Length of stay (days)",
                   outcome_death_30d = "Outcome at day 30"),
  groups = list("", "ICU outcome"))
strata <- c(list(overall=Pop1), split(Pop1, Pop1$outcome_icu_death))
table1(strata, labels, groupspan = c(1, 2))


####################################################
### different statistics for different variables ###
####################################################
## CAVE: falsche / vertauschte Werte sind noch nicht korrigiert!

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 age_at_admission = "Median (Q1-Q3)",
                 height_at_admission = "Median (Q1-Q3)",
                 weight_at_admission = "Median (Q1-Q3)",
                 BMI = "Median (Q1-Q3)",
                 apache_score = "Median (Q1-Q3)",
                 length_of_stay = "Median (Q1-Q3)")
  parse.abbrev.render.code(c("", what))(x)
}
table1(strata, labels, groupspan=c(1,2), render=rndr)

