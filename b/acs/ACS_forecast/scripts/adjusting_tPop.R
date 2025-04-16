library(writexl)

tpop = cps_website_raw %>% mutate(educ = ifelse(educ == 0, 9, educ)) %>%
  arrange(year, race, sex, age, educ)


for(i in 0:1759){
  totpop_2 = sum(tpop$tpop_2[(5*i + 1):(5*i + 4)])
  if(is.na(totpop_2) || totpop_2 == 0) next
  tpop$tpop_2[5*i + 1] = tpop$tpop_2[5*i + 1] + (tpop$tpop_2[5*i + 1]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 2] = tpop$tpop_2[5*i + 2] + (tpop$tpop_2[5*i + 2]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 3] = tpop$tpop_2[5*i + 3] + (tpop$tpop_2[5*i + 3]/totpop_2)*tpop$tpop_2[5*i + 5]
  tpop$tpop_2[5*i + 4] = tpop$tpop_2[5*i + 4] + (tpop$tpop_2[5*i + 4]/totpop_2)*tpop$tpop_2[5*i + 5]
}
tpop = tpop %>% filter(educ != 9)

# merge_non_inst.R

# Load required package
library(dplyr)

# ---- Assumes both tpop_clean and prep_novosad_grouped are already loaded ----

# Check structure (optional sanity check)
# str(tpop_clean)
# str(prep_novosad_grouped)

library(dplyr)

# Merge non_inst into tpop_clean by common identifiers
tpop_merged <- tpop_clean %>%
  left_join(
    prep_novosad_grouped %>%
      select(year, age, edclass, sex, race, non_inst),
    by = c("year", "age", "edclass", "sex", "race")
  )

data_seperated <- website_data_seperated %>%
  left_join(
    tpop_merged %>%
      select(year, age, edclass, sex, race, total_non_inst_k),
    by = c("year", "age", "edclass", "sex", "race")
  ) %>%
  filter(year != 2000)

library(dplyr)

library(dplyr)

# Define objective function to minimize MSE
compute_mse <- function(x, data) {
  data_adj <- data
  
  # Filter rows with edclass 0
  ed0_rows <- data %>% filter(edclass == 0)
  
  # Redistribute tpop_2 to edclass 1 and 2
  redistributed <- ed0_rows %>%
    select(year, race, sex, age, tpop_2) %>%
    mutate(
      tpop_2_ed1 = x * tpop_2,
      tpop_2_ed2 = (1 - x) * tpop_2
    ) %>%
    select(-tpop_2)
  
  # Merge and adjust tpop_2 in edclass 1 and 2
  data_adj <- data_adj %>%
    filter(edclass != 0) %>%
    left_join(redistributed %>% select(year, race, sex, age, tpop_2_ed1),
              by = c("year", "race", "sex", "age")) %>%
    left_join(redistributed %>% select(year, race, sex, age, tpop_2_ed2),
              by = c("year", "race", "sex", "age"),
              suffix = c("", "_ed2")) %>%
    mutate(
      tpop_2_ed1 = ifelse(edclass == 1, coalesce(tpop_2_ed1, 0), 0),
      tpop_2_ed2 = ifelse(edclass == 2, coalesce(tpop_2_ed2, 0), 0),
      tpop_2 = tpop_2 + tpop_2_ed1 + tpop_2_ed2
    )
  
  # Compute MSE
  mse <- data_adj %>%
    filter(edclass %in% 1:4, !is.na(total_non_inst_k)) %>%
    summarise(
      mse = mean((tpop_2 - total_non_inst_k)^2)
    ) %>%
    pull(mse)
  
  return(mse)
}

data_seperated_filtered <- data_seperated %>% filter(year != 2019, year != 2020, year != 2021)

# Optimize x in [0, 1] to minimize MSE
opt_result <- optimize(
  f = compute_mse,
  interval = c(0, 1),
  data = data_seperated_filtered
)

# Show optimal x and MSE
print(opt_result)

# Apply optimal x to build adjusted dataset
data_seperated <- final_data

x_opt <- 2/3

ed0_rows <- data_seperated %>% filter(edclass == 0)

redistributed <- ed0_rows %>%
  select(year, race, sex, age, tpop_2) %>%
  mutate(
    tpop_2_ed1 = x_opt * tpop_2,
    tpop_2_ed2 = (1 - x_opt) * tpop_2
  ) %>%
  select(-tpop_2)

# Create adjusted data
data_adjusted <- data_seperated %>%
  filter(edclass != 0) %>%
  left_join(redistributed %>% select(year, race, sex, age, tpop_2_ed1),
            by = c("year", "race", "sex", "age")) %>%
  left_join(redistributed %>% select(year, race, sex, age, tpop_2_ed2),
            by = c("year", "race", "sex", "age"),
            suffix = c("", "_ed2")) %>%
  mutate(
    tpop_2_ed1 = ifelse(edclass == 1, coalesce(tpop_2_ed1, 0), 0),
    tpop_2_ed2 = ifelse(edclass == 2, coalesce(tpop_2_ed2, 0), 0),
    tpop_2 = tpop_2 + tpop_2_ed1 + tpop_2_ed2
  )

# View result
head(data_adjusted)

# Create final adjusted dataset
data_adjusted <- data_seperated %>%
  filter(edclass != 0) %>%
  left_join(
    redistributed %>% select(year, race, sex, age, tpop_2_ed1),
    by = c("year", "race", "sex", "age")
  ) %>%
  left_join(
    redistributed %>% select(year, race, sex, age, tpop_2_ed2),
    by = c("year", "race", "sex", "age"),
    suffix = c("", "_ed2")
  ) %>%
  mutate(
    tpop_2_ed1 = ifelse(edclass == 1, coalesce(tpop_2_ed1, 0), 0),
    tpop_2_ed2 = ifelse(edclass == 2, coalesce(tpop_2_ed2, 0), 0),
    tpop_2 = tpop_2 + tpop_2_ed1 + tpop_2_ed2
  ) 

# Finalize dataset: remove redistribution cols and add share_non_inst
data_adjusted <- data_adjusted %>%
  select(-tpop_2_ed1, -tpop_2_ed2) %>%
  mutate(
    share_non_inst = ifelse(tpop_2 > 0 & !is.na(total_non_inst_k),
                            (total_non_inst_k / tpop_2) * 100,
                            NA)
  )

library(dplyr)

share_stats_by_year <- data_adjusted %>%
  filter(!is.na(share_non_inst)) %>%
  group_by(year) %>%
  summarise(
    mean_share = mean(share_non_inst, na.rm = TRUE),
    var_share = var(share_non_inst, na.rm = TRUE),
    sum_tpop_2 = sum(tpop_2, na.rm = TRUE),
    sum_non_inst_k = sum(total_non_inst_k, na.rm = TRUE),
    pct_below_90 = mean(share_non_inst < 90, na.rm = TRUE) * 100,
    pct_above_110 = mean(share_non_inst > 110, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(share_stats_by_year)

share_stats_by_year2 <- tpop_merged %>%
  filter(!is.na(share_non_inst)) %>%
  group_by(year) %>%
  summarise(
    mean_share = mean(share_non_inst, na.rm = TRUE),
    var_share = var(share_non_inst, na.rm = TRUE),
    sum_tpop_2 = sum(total_tpop_2, na.rm = TRUE),
    sum_non_inst_k = sum(total_non_inst_k, na.rm = TRUE),
    .groups = "drop"
  )

print(share_stats_by_year)

# View adjusted data
head(data_adjusted)

# Optional: Save result to a new object or file
# saveRDS(tpop_merged, "tpop_with_non_inst.rds")
# write.csv(tpop_merged, "tpop_with_non_inst.csv", row.names = FALSE)

library(dplyr)

# Summarize by race, sex, and edclass
summary_by_group <- tpop_merged %>%
  filter(year == 2014) %>%
  group_by(year, race, sex, edclass, age) %>%
  summarise(
    total_tpop_original = sum(total_tpop_2, na.rm = TRUE),
    total_tpop_new = sum(tpop_2, na.rm = TRUE),
    total_non_inst_k = sum(total_non_inst_k, na.rm = TRUE),
    share_non_inst_old = total_non_inst_k * 100 / total_tpop_original,
    share_non_inst_new = total_non_inst_k * 100 / total_tpop_new,
    .groups = "drop"
  )

print(summary_by_group)

library(dplyr)

# Step 1: Assign 5-year age groups
prep_acs_cohorts <- prep_acs_granage %>%
  mutate(
    age_group = cut(
      age,
      breaks = seq(25, 70, by = 5),
      right = FALSE,
      labels = c("25-29", "30-34", "35-39", "40-44", "45-49",
                 "50-54", "55-59", "60-64", "65-69")
    )
  )

# Step 2: Group and sum inst per cohort
prep_acs_grouped <- prep_acs_cohorts %>%
  group_by(year, age_group, edclass, race, sex) %>%
  summarise(
    inst = sum(inst, na.rm = TRUE),
    .groups = "drop"
  )

# View the resulting dataset
head(prep_acs_grouped)

library(dplyr)

# Step 1: Filter out year 2000 from the grouped ACS data
prep_acs_grouped_filtered <- prep_acs_grouped %>%
  filter(year > 2000) %>%
  rename(age = age_group)  %>%
  mutate(inst = inst / 1000)


# Step 2: Merge inst into data_adjusted
data_adjusted <- data_adjusted %>%
  left_join(
    prep_acs_grouped_filtered,
    by = c("year", "age", "edclass", "race", "sex")
  )

# Step 3: Calculate inst_rate (only for years <= 2018)
data_adjusted <- data_adjusted %>%
  mutate(
    inst_rate = ifelse(year <= 2018 & !is.na(inst.y) & tpop_2 > 0,
                       (inst.y) / (inst.y + tpop_2),
                       NA)
  )

library(dplyr)

# Step 1: Extract inst_rate from 2018
inst_rate_2018 <- data_adjusted %>%
  filter(year == 2018, !is.na(inst_rate)) %>%
  select(age, edclass, sex, race, inst_rate_2018 = inst_rate)

library(dplyr)

# Step 1: Extract 2018 inst_rate per age/edclass/sex/race group
inst_rate_2018 <- data_adjusted %>%
  filter(year == 2018, !is.na(inst_rate)) %>%
  select(age, edclass, sex, race, inst_rate_2018 = inst_rate)

# Step 2: Join into 2019–2021 and assign inst_rate
data_adjusted <- data_adjusted %>%
  left_join(inst_rate_2018, by = c("age", "edclass", "sex", "race")) %>%
  mutate(
    inst_rate = ifelse(
      year %in% 2019:2021 & !is.na(inst_rate_2018),
      inst_rate_2018,
      inst_rate  # keep original for 2001–2018
    )
  ) %>%
  select(-inst_rate_2018)  # clean up temporary column

data_adjusted <- data_adjusted %>%
  mutate(
    inst.x = ifelse(
      year %in% 2019:2021 & !is.na(inst_rate) & !is.na(tpop_2),
      inst_rate * tpop_2,
      NA
    )
  )
data_adjusted <- data_adjusted %>%
  mutate(
    inst.x = ifelse(is.na(inst.x) & !is.na(inst.y), inst.y, inst.x)
  )

obs_by_year_sex_race <- data_adjusted %>%
  count(year, sex, race, name = "n_obs")

print(obs_by_year_sex_race)

tpop_0121 <- data_adjusted %>%
  rename(
    non_inst = tpop_2,
    inst = inst.x
  ) %>%
  mutate(
    tpop = non_inst + inst
  ) %>%
  select(
    -total_non_inst_k,
    -share_non_inst,
    -inst.y
  )

write.csv(
  tpop_0121,
  file = "/Users/Stata/SeminarHealthSociety/acs_files/output/Census extrapolation until 2005/Output censes extrapolation until 2005/tpop_0121.csv",
  row.names = FALSE
)

library(dplyr)

# Step 1: Rename age_group to age in prep_acs_grouped
prep_acs_grouped <- prep_acs_grouped %>%
  rename(age = age_group)

# Step 2: Join inst from prep_acs_grouped into data_adjusted
data_adjusted <- data_adjusted %>%
  left_join(
    acs_grouped %>% select(year, age, sex, race, edclass, inst),
    by = c("year", "age", "sex", "race", "edclass"),
    suffix = c("", "_acs")  # avoids overwriting if inst already exists
  )

library(dplyr)

# Step 1–3: Clean up, scale tpop_2, and compute inst_rate
data_adjusted <- data_adjusted %>%
  select(-inst.x, -inst.y) %>%  # Remove inst.x and inst.y
  mutate(
    tpop_2 = tpop_2 * 1000,  # Scale tpop_2
    inst_rate = ifelse(!is.na(inst) & tpop_2 > 0,
                       inst / (inst + tpop_2),
                       NA)
  )

# Step 4: Extract 2018 inst_rate as reference
inst_rate_2018 <- data_adjusted %>%
  filter(year == 2018, !is.na(inst_rate)) %>%
  select(age, sex, race, edclass, inst_rate_2018 = inst_rate)

# Step 5: Join and apply to 2019–2021
data_adjusted <- data_adjusted %>%
  left_join(inst_rate_2018, by = c("age", "sex", "race", "edclass")) %>%
  mutate(
    inst_rate = ifelse(year %in% 2019:2021 & !is.na(inst_rate_2018),
                       inst_rate_2018,
                       inst_rate)
  ) %>%
  select(-inst_rate_2018)  # Cleanup

library(dplyr)

acs_grouped <- acs_grouped %>%
  # Step 1: Remove observations where wbho or sex is 0
  filter(wbho != 0, sex != 0) %>%
  
  # Step 2: Remove unnecessary columns
  select(-inst_2000, -inst_2006) %>%
  
  # Step 3: Rename variables
  rename(
    race = wbho,
    age = age_gp
  ) %>%
  
  # Step 4: Filter to keep only age == 70
  filter(age == 70 | age %in% seq(25, 65, by = 5)) %>%
  
  # Step 5: Convert age to 5-year age group labels
  mutate(
    age = case_when(
      age == 25 ~ "25-29",
      age == 30 ~ "30-34",
      age == 35 ~ "35-39",
      age == 40 ~ "40-44",
      age == 45 ~ "45-49",
      age == 50 ~ "50-54",
      age == 55 ~ "55-59",
      age == 60 ~ "60-64",
      age == 65 ~ "65-69",
      age == 70 ~ "70",  # keep as "70" if you want to preserve it
      TRUE ~ as.character(age)
    )
  )