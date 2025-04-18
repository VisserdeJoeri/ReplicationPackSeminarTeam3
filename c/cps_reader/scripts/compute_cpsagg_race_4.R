# This programme creates race 4, the difference between the total population and race 1-3

compute_cpsagg_race_4 <- function(final_data) {
  # Divide dataset in dataset with total numbers and dataset with known race 1-3
  race_total <- final_data %>% filter(race == 0)
  race_subgroups <- final_data %>% filter(race %in% c(1, 2, 3))
  
  # Sum the total population of races 1, 2, and 3 per group (year, sex, age, educ)
  race_subgroup_totals <- race_subgroups %>%
    group_by(year, sex, age, educ) %>%
    summarise(total_known_races = sum(tpop_2, na.rm = TRUE), .groups = "drop")
  
  # Race 4 population: total (race 0) - (race 1 + race 2 + race 3)
  race_4_data <- left_join(race_total, race_subgroup_totals, by = c("year", "sex", "age", "educ")) %>%
    mutate(
      tpop_2 = tpop_2 - total_known_races,  
      tpop_2 = ifelse(tpop_2 < 0, 0, tpop_2),  # Very rarely, tpop2 is negative. We set it to 0
      race = 4
    ) %>%
    select(year, race, sex, age, educ, tpop_2)  # Throw unnessary columns out
  
  # Add race 4 to the full dataset
  final_data <- bind_rows(final_data, race_4_data)
  
  return(final_data)
}