# scripts/load_cps_novosad.R
# Clean CPS Novosad (granular age) dataset

prep_cps_novosad <- cps_novosadgranage_raw %>%
  filter(year >= 2000, year <= 2018) %>%
  filter(wbho != 0, sex != 0, age < 70) %>%
  rename(race = wbho)

# That's it — cps_novosadgranage_raw is now cleaned and ready