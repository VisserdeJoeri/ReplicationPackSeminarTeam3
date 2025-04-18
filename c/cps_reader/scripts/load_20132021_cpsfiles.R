# This function reads Excel files from folders (each corresponding to a race),
# extracts data, and combines them into one cleaned dataset.

load_20132021_cpsfiles <- function(base_folder, race_folders) {
  final_data <- data.frame()
  
  # Loop through each race folder
  for (folder in names(race_folders)) {
    race_value <- race_folders[[folder]]  
    input_folder <- file.path(base_folder, folder)
    
    # List all Excel files in the folder
    file_list <- list.files(
      input_folder,
      pattern = "\\.xls[x]?$",
      full.names = TRUE
    )
    
    # Loop over each file in the folder
    for (file_path in file_list) {
      df <- readxl::read_excel(file_path, col_names = FALSE)
      
      year_text <- as.character(df[2, 1])
      year_value <- stringr::str_extract(year_text, "200[0-9]|201[0-9]|202[0-1]") %>% as.numeric()
      
      if (is.na(year_value)) year_value <- 1901
      
      # We only need rows 26–35 (men) and 41–50 (women). I have checked every file to match this.
      selected_rows <- c(26:35, 41:50)
      df_data <- df[selected_rows, ]
      
      # Give placeholder column names
      colnames(df_data) <- c("age", paste0("col_", 2:ncol(df_data)))
      
      # Remove rows with missing age
      df_data <- dplyr::filter(df_data, !is.na(age))
      
      # Convert numeric columns from character to numeric
      suppressWarnings({
        df_data[, -1] <- lapply(df_data[, -1], function(x) as.numeric(as.character(x)))
      })
      
      # Add a helper column to identify sex
      df_data$row_index <- selected_rows
      df_data <- df_data %>%
        dplyr::mutate(sex = dplyr::case_when(
          row_index >= 26 & row_index <= 35 ~ 1,
          row_index >= 41 & row_index <= 50 ~ 2
        )) %>%
        dplyr::filter(!is.na(sex))
      
      # Map education columns to education levels. 11-12th grade is assigned code 0, to be assigned later.
      educ_mapping <- c(
        "col_3" = 0,
        "col_4" = 1, "col_5" = 1, "col_6" = 1, "col_7" = 1, "col_8" = 1, "col_9" = 1,
        "col_10" = 2,
        "col_11" = 3, "col_12" = 3, "col_13" = 3,
        "col_14" = 4, "col_15" = 4, "col_16" = 4, "col_17" = 4
      )
      
      # Turn wide format into long format: one row per (age, sex, education)
      df_long <- df_data %>%
        tidyr::pivot_longer(
          cols = starts_with("col_"),
          names_to = "educ_col",
          values_to = "tpop_2"
        ) %>%
        dplyr::mutate(
          educ = educ_mapping[educ_col]
        ) %>%
        dplyr::filter(!is.na(educ))
      
      # Add year and race
      df_long <- df_long %>%
        dplyr::mutate(
          year = year_value,
          race = race_value
        ) %>%
        dplyr::select(year, race, sex, age, educ, tpop_2)
      
      # Group by the key variables and sum up population
      df_final <- df_long %>%
        dplyr::group_by(year, race, sex, age, educ) %>%
        dplyr::summarise(tpop_2 = sum(tpop_2, na.rm = TRUE), .groups = "drop")
      
      # Add to final dataset
      final_data <- dplyr::bind_rows(final_data, df_final)
    }
  }
  
  return(final_data)
}