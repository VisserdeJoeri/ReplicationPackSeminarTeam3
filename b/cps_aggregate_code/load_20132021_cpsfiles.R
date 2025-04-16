load_20132021_cpsfiles <- function(base_folder, race_folders) {
  final_data <- data.frame()
  
  for (folder in names(race_folders)) {
    race_value <- race_folders[[folder]]
    input_folder <- file.path(base_folder, folder)
    
    cat("Reading from folder:", input_folder, "\n")
    
    file_list <- list.files(
      input_folder,
      pattern = "\\.xls[x]?$",
      full.names = TRUE
    )
    file_list <- file_list[!grepl("~\\$", basename(file_list))]
    
    if (length(file_list) == 0) next
    
    for (file_path in file_list) {
      df <- read_excel(file_path, col_names = FALSE)
      year_text <- as.character(df[2, 1])
      print(basename(file_path))
      print(year_text)
      year_value <- str_extract(year_text, "200[0-9]|201[0-9]|202[0-1]") %>% as.numeric()
      
      if (is.na(year_value)) {
        year_value <- 1901
        cat("\u26a0\ufe0f  Year not found in", basename(file_path), "- using 1901 as fallback.\n")
      }
      
      selected_rows <- c(26:35, 41:50)
      df_data <- df[selected_rows, ]
      
      colnames(df_data) <- c("age", paste0("col_", 2:ncol(df_data)))
      df_data <- df_data %>% filter(!is.na(age))
      
      suppressWarnings({
        df_data[, -1] <- lapply(df_data[, -1], function(x) as.numeric(as.character(x)))
      })
      
      df_data$row_index <- selected_rows
      df_data <- df_data %>%
        mutate(sex = case_when(
          row_index >= 26 & row_index <= 35 ~ 1,
          row_index >= 41 & row_index <= 50 ~ 2,
          TRUE ~ NA_real_
        )) %>%
        filter(!is.na(sex))
      
      educ_mapping <- c("col_3" = 1, "col_4" = 1, "col_5" = 1, "col_6" = 1, "col_7" = 1, "col_8" = 1,
                        "col_9" = 0, "col_10" = 2, "col_11" = 3, "col_12" = 3, "col_13" = 3,
                        "col_14" = 4, "col_15" = 4, "col_16" = 4, "col_17" = 4)
      
      df_long <- df_data %>%
        pivot_longer(cols = starts_with("col_"), names_to = "educ_col", values_to = "tpop_2") %>%
        mutate(educ = educ_mapping[educ_col]) %>%
        filter(!is.na(educ))
      
      df_long <- df_long %>%
        mutate(year = year_value, race = race_value) %>%
        select(year, race, sex, age, educ, tpop_2)
      
      df_final <- df_long %>%
        group_by(year, race, sex, age, educ) %>%
        summarise(tpop_2 = sum(tpop_2, na.rm = TRUE), .groups = "drop")
      
      final_data <- bind_rows(final_data, df_final)
    }
  }
  
  return(final_data)
}