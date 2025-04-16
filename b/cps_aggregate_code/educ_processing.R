redistribute_educ0 <- function(df, x_opt) {
  educ0 <- df %>% filter(educ == 0)
  df_non0 <- df %>% filter(educ != 0)
  
  if (nrow(educ0) == 0) return(df)
  
  redistribution <- educ0 %>%
    group_by(year, race, sex, age) %>%
    summarise(tpop_2 = sum(tpop_2, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      educ1 = x_opt * tpop_2,
      educ2 = (1 - x_opt) * tpop_2
    ) %>%
    select(year, race, sex, age, educ1, educ2) %>%
    pivot_longer(cols = c("educ1", "educ2"), names_to = "educ_label", values_to = "tpop_2") %>%
    mutate(educ = ifelse(educ_label == "educ1", 1, 2)) %>%
    select(year, race, sex, age, educ, tpop_2)
  
  df_final <- bind_rows(df_non0, redistribution) %>%
    group_by(year, race, sex, age, educ) %>%
    summarise(tpop_2 = sum(tpop_2, na.rm = TRUE), .groups = "drop")
  
  return(df_final)
}
