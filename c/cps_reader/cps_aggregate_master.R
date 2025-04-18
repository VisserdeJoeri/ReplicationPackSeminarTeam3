# This is the master file to read the public CPS files. 
# The end product is an estimate for the 2000-2021 non_inst population by age, race, sex and edclass

# Load required libraries
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

# Define paths for subprograms
subprogram_path <- "/Users/Stata/SeminarHealthSociety/c/cps_reader/scripts"

# Source subprograms
source(paste0(subprogram_path, "load_20132021_cpsfiles.R"))
source(paste0(subprogram_path, "compute_cpsagg_race_4.R"))

# Define parent directory and subfolders for each race
base_folder_xslx <- "/Users/Stata/SeminarHealthSociety/data/cps_files"
race_folders <- c("race_1" = 1, "race_2" = 2, "race_3" = 3, "race_0" = 0)

# Output file
list.files(file.path(base_folder_xslx, "race_1"))
list.files("/Users/Stata/data/raw/cps_aggregate/race_1", pattern = "\\.xls[x]?$", full.names = TRUE)
output_file <- "'/Users/Stata/SeminarHealthSociety/data/output/(non)inst_estimates/non_inst.csv"

# Load and process all files
final_data <- load_20132021_cpsfiles(base_folder_xslx, race_folders)

#Compute and append Race 4 data
final_data <- compute_cpsagg_race_4(final_data)

# Last checks: Ensure `tpop_2` is non-negative and remove Race 0
final_data <- final_data %>% mutate(tpop_2 = pmax(0, tpop_2)) %>% filter(race != 0)

# Save the processed CSV file
write.csv(final_data, output_file, row.names = FALSE)