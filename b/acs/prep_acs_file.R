# Load necessary libraries
library(dplyr)
library(stringr)

# 1. Filter `processed_all_races` for years 2010 to 2018
processed_subset <- processed_all_races %>%
  filter(year >= 2010 & year <= 2018)

# 2. Transform `inst_pops` dataset
library(dplyr)

inst_pops_transformed <- inst_pops %>%
  # Rename variables
  rename(
    educ = edclass,
    age = age_gp,
    race = wbho
  ) %>%
  # Filter out rows where race, sex, or educ is 0
  filter(race != 0, sex != 0, educ != 0) %>%
  # Convert numeric age to age groups
  mutate(
    age = case_when(
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85            ~ "85+",
      TRUE ~ NA_character_
    ),
    # Divide population (or relevant column) by 1000
    inst = inst / 1000
  )

# Merge datasets including year as a matching variable
merged_data <- processed_subset %>%
  left_join(inst_pops_transformed, 
            by = c("age", "race", "educ", "sex", "year")) %>%
  # Replace NA in inst with 0 for unmatched rows
  mutate(inst = ifelse(is.na(inst), 0, inst)) %>%
  # Select relevant columns
  select(age, race, educ, sex, year, tpop_2, inst)

merged_data <- merged_data %>%
  mutate(inst_pct = (inst / tpop_2) * 100)

