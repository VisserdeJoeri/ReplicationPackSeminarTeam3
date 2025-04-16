# scripts/load_cps_website.R
# Clean cps_website_raw and redistribute edclass = 0 based on group proportions

library(dplyr)

# Step 1: Filter and rename
cps_website_clean <- cps_website_raw %>%
  filter(
    year <= 2018,
    age != "70-74"
  ) %>%
  rename(edclass = educ, tpop2 = tpop_2)

# Step 2: Separate edclass = 0 and edclass 1–4
cps_valid_ed <- cps_website_clean %>%
  filter(edclass %in% 1:4)

cps_ed0 <- cps_website_clean %>%
  filter(edclass == 0)

# Step 3: Compute group-wise distribution proportions (for edclass 1–4)
group_props <- cps_valid_ed %>%
  group_by(year, age, sex, race) %>%
  mutate(total_group_tpop = sum(tpop2)) %>%
  filter(total_group_tpop > 0) %>%
  mutate(prop = tpop2 / total_group_tpop) %>%
  select(year, age, sex, race, edclass, prop)

# Step 4: Join edclass=0 with the proportions
ed0_distributed <- cps_ed0 %>%
  select(year, age, sex, race, tpop2) %>%
  left_join(group_props, by = c("year", "age", "sex", "race")) %>%
  mutate(tpop2 = tpop2 * prop) %>%
  filter(!is.na(tpop2)) %>%
  select(year, age, sex, race, edclass, tpop2)

# Step 5: Combine back the redistributed data with the original 1–4 edclass rows
prep_cps_website <- cps_valid_ed %>%
  select(year, age, sex, race, edclass, tpop2) %>%
  bind_rows(ed0_distributed)

# Optional: sort rows for readability
prep_cps_website <- prep_cps_website %>%
  arrange(year, age, sex, race, edclass)

# Final dataset is: prep_cps_website