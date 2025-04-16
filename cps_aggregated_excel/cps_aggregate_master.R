# Load required libraries
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)


#I got errors so that's why I'm putting the transform function here
transform_age <- function(age) {
  age_map <- c(
    "..25 to 29 years" = "25-29",
    "..30 to 34 years" = "30-34",
    "..35 to 39 years" = "35-39",
    "..40 to 44 years" = "40-44",
    "..45 to 49 years" = "45-49",
    "..50 to 54 years" = "50-54",
    "..55 to 59 years" = "55-59",
    "..60 to 64 years" = "60-64",
    "..65 to 69 years" = "65-69",
    "..70 to 74 years" = "70-74"
  )
  
  return(ifelse(age %in% names(age_map), age_map[age], age))
}

# Define paths for subprograms
subprogram_path <- "/Users/Stata/b/cps_aggregate_code/"

# Source subprograms
source(paste0(subprogram_path, "load_20132021_cpsfiles.R"))
source(paste0(subprogram_path, "compute_cpsagg_race_4.R"))
source(paste0(subprogram_path, "educ_processing.R"))

# Get base folder and subfolders for each race
base_folder_xslx <- "/Users/Stata/data/raw/cps_aggregate/2013_2021_xslx_2/"
race_folders <- c("race_1" = 1, "race_2" = 2, "race_3" = 3, "race_0" = 0)  # Race 0 is total

# Output file
list.files(file.path(base_folder_xslx, "race_1"))
list.files("/Users/Stata/data/raw/cps_aggregate/race_1", pattern = "\\.xls[x]?$", full.names = TRUE)
output_file <- "/Users/friso/Documents/Stata/Results/processed_all_races.csv"

# Load and process all files
final_data <- load_20132021_cpsfiles(base_folder_xslx, race_folders)
final_data <- redistribute_educ0(final_data, 2/3)

#Compute race 4 data, remove race = 0
final_data <- compute_cpsagg_race_4(final_data)
final_data <- final_data %>% mutate(tpop_2 = pmax(0, tpop_2)) %>% filter(race != 0, age != "..70 to 74 years") %>% rename(edclass = educ)

# Transform age for later processing
final_data$age <- sapply(final_data$age, transform_age)

# Save the processed CSV file
write.csv(final_data, output_file, row.names = FALSE)

print(paste("Merged processed file saved as:", output_file))
