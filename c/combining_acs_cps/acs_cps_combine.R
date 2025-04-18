# This dataset is for combining cps (non_inst) and acs (inst) dataset 
library(tidyverse)
library(haven)
library(readr)

# Load the datasets
cps_path <- "/Users/friso/Documents/Stata/Results/processed_all_races.csv"
acs_path <- "/Users/friso/Documents/Stata/extrapolated_inst_pops_2000_2005.dta"

old_cps_dataset <- read_csv(cps_path)
old_acs_dataset <- read_dta(acs_path)

output_file <- "/Users/friso/Documents/Stata/Results/tpop_0021.csv"

# Step 1: Prep the ACS dataset
# Renaming some columns to match the CPS dataset
acs_dataset <- old_acs_dataset %>%
  rename(race = wbho, age = age_gp) %>%  
  select(-inst_2000, -inst_2006) %>%     
  mutate(
    age = case_when(
      age == 25 ~ "25-29",
      age == 30 ~ "30-34",
      age == 35 ~ "35-39",
      age == 40 ~ "40-44",
      age == 45 ~ "45-49",
      age == 50 ~ "50-54",
      age == 55 ~ "55-59",
      age == 60 ~ "60-64",
      age == 65 ~ "65-69",
      age == 70 ~ "70-74",
      TRUE ~ as.character(age)
    )
  ) %>%
  filter(age != "70-74", race != 0, sex != 0)  

# Step 2: Combine CPS with ACS

# Merge the two datasets using the same groups
combined_dataset <- left_join(
  cps_dataset,
  acs_dataset,
  by = c("year", "age", "race", "sex", "edclass")
)

# Step 3: Calculate institutionalization rates and fill missing years
# Compute inst_rate for 2000-2018
combined_dataset <- combined_dataset %>%
  mutate(
    inst_rate = if_else(
      year <= 2018,
      inst / (inst + non_inst),
      NA_real_
    )
  )

# Filtering the 2018 rates
inst_rate_2018 <- combined_dataset %>%
  filter(year == 2018) %>%
  select(age, race, sex, edclass, inst_rate)

# Join the 2018 rates to 2019-2021 rows
combined_dataset <- combined_dataset %>%
  left_join(
    inst_rate_2018,
    by = c("age", "race", "sex", "edclass"),
    suffix = c("", "_2018")
  ) %>%
  mutate(
    # If inst_rate is missing (i.e., years 2019-2021), use the 2018 version
    inst_rate = if_else(is.na(inst_rate) & year > 2018, inst_rate_2018, inst_rate),
    # Calculate inst using the estimated inst_rate
    inst = if_else(year > 2018, inst_rate * non_inst / (1 - inst_rate), inst)
  ) %>%
  select(-inst_rate_2018)

write.csv(cps_dataset, output_file, row.names = FALSE)
