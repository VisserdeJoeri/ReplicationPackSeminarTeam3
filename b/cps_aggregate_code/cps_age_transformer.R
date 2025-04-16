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