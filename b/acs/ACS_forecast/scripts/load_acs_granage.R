# scripts/load_cps_novosad.R
# Clean CPS Novosad (granular age) dataset

prep_acs_granage <- acs_granage_raw %>%
  filter(wbho != 0, sex != 0, age < 70) %>%
  rename(race = wbho) %>%
  select(-inst_2000, -inst_2006)
