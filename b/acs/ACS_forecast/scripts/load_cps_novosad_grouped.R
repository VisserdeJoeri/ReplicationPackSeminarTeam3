# scripts/load_cps_novosad_grouped.R
# Clean CPS Novosad grouped-age dataset

prep_novosad_grouped <- cps_novosad_raw %>%
  filter(
    year >= 2000,
    wbho != 0,
    sex != 0,
    age_gp < 70
  ) %>%
  mutate(
    age = case_when(
      age_gp == 25 ~ "25-29",
      age_gp == 30 ~ "30-34",
      age_gp == 35 ~ "35-39",
      age_gp == 40 ~ "40-44",
      age_gp == 45 ~ "45-49",
      age_gp == 50 ~ "50-54",
      age_gp == 55 ~ "55-59",
      age_gp == 60 ~ "60-64",
      age_gp == 65 ~ "65-69",
      TRUE ~ NA_character_  # safety fallback
    )
  )