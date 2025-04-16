# diagnostics.R

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(tsibble)
library(urca)

# Filter out year 2000
data <- combined_acs_cps %>%
  filter(year > 2000) %>%
  mutate(birth_year = year - age)

# Summarize total institutionalized and non-institutionalized per year
yearly_totals <- data %>%
  group_by(year) %>%
  summarise(
    total_inst = sum(inst, na.rm = TRUE),
    total_non_inst = sum(non_inst, na.rm = TRUE)
  )

# Visualize development over time
yearly_totals_long <- yearly_totals %>%
  pivot_longer(cols = c(total_inst, total_non_inst),
               names_to = "type",
               values_to = "count")

ggplot(yearly_totals_long, aes(x = year, y = count, color = type)) +
  geom_line(size = 1.2) +
  labs(title = "Institutionalized vs Non-Institutionalized Over Time",
       x = "Year", y = "Population Count") +
  theme_minimal()

# Compute group-wise percentual change in inst and non_inst
library(dplyr)
library(ggplot2)

# Define custom high-contrast colors for race codes 1–4
race_colors <- c(
  "1" = "#1b9e77",  # Teal-green
  "2" = "#d95f02",  # Burnt orange
  "3" = "#7570b3",  # Indigo purple
  "4" = "#e7298a"   # Magenta pink
)

# Process the data
group_changes <- combined_acs_cps %>%
  filter(year > 2000) %>%
  mutate(
    birth_year = year - age,
    race = as.factor(race)  # Make race a factor for coloring
  ) %>%
  filter(birth_year >= 1953, birth_year <= 1972, race != "4") %>%
  mutate(
    cohort = case_when(
      birth_year >= 1953 & birth_year <= 1957 ~ "1953–1957",
      birth_year >= 1958 & birth_year <= 1962 ~ "1958–1962",
      birth_year >= 1963 & birth_year <= 1967 ~ "1963–1967",
      birth_year >= 1968 & birth_year <= 1972 ~ "1968–1972",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cohort)) %>%
  group_by(cohort, edclass, sex, race) %>%
  arrange(year) %>%
  mutate(
    total_pop = inst + non_inst,
    lag_total_pop = lag(total_pop),
    diff_total_pop = total_pop - lag_total_pop,
    pct_change_total_pop = 100 * diff_total_pop / lag_total_pop,
    pct_change_total_pop = ifelse(is.infinite(pct_change_total_pop) | is.nan(pct_change_total_pop), NA, pct_change_total_pop)
  ) %>%
  ungroup()


library(slider)

group_changes <- group_changes %>%
  group_by(cohort, edclass, sex, race) %>%
  arrange(year) %>%
  mutate(
    pct_change_total_pop_3yr = slide_dbl(
      pct_change_total_pop,
      mean,
      .before = 1, .after = 1,
      .complete = TRUE,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

ggplot(group_changes %>% 
         filter(!is.na(pct_change_total_pop_3yr), abs(pct_change_total_pop_3yr) < 500),
       aes(x = pct_change_total_pop_3yr, fill = race, color = race)) +
  geom_density(alpha = 0.4) +
  geom_jitter(aes(y = 0), height = 0.01, alpha = 0.2, size = 1.2, shape = 16) +
  scale_fill_manual(values = race_colors) +
  scale_color_manual(values = race_colors) +
  labs(
    title = "3-Year Averaged Percent Change in Group Population",
    subtitle = "Centered smoothing (years t-1, t, t+1) — Colored by Race",
    x = "3-Year Averaged % Change",
    y = "Density",
    fill = "Race",
    color = "Race"
  ) +
  theme_minimal(base_size = 13)

# Plot the percent change in total population
ggplot(group_changes %>% filter(abs(pct_change_total_pop) < 500),
       aes(x = year, y = pct_change_total_pop,
           group = interaction(cohort, edclass, sex, race),
           color = race)) +
  geom_line(alpha = 0.5, linewidth = 0.8) +
  facet_wrap(~sex, nrow = 2) +
  scale_color_manual(values = race_colors) +
  labs(
    title = "Percent Change in Total Group Population by Cohort (1953–1972)",
    subtitle = "Grouped by 5-year birth cohort, education class, race, and sex",
    x = "Year",
    y = "% Change from Previous Year",
    color = "Race"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Add a normal curve with same mean/sd (for illustration only)
library(ggplot2)

# Filter extreme outliers and missing values
distribution_data <- group_changes %>%
  filter(
    !is.na(pct_change_total_pop),
    abs(pct_change_total_pop) < 500
  )

# Density plot of percent change, colored by race
# Add a normal curve with same mean/sd (for illustration only)
ggplot(distribution_data, aes(x = pct_change_total_pop, fill = race, color = race)) +
  geom_density(alpha = 0.4) +
  geom_jitter(aes(y = 0), height = 0.01, alpha = 0.2, size = 1.2, shape = 16) +
  scale_fill_manual(values = race_colors) +
  scale_color_manual(values = race_colors) +
  labs(
    title = "Distribution of Percent Change in Group Population (with Observations)",
    subtitle = "Jittered dots represent individual cohort-year-group changes",
    x = "% Change from Previous Year",
    y = "Density",
    fill = "Race",
    color = "Race"
  ) +
  theme_minimal(base_size = 13)

ggplot(group_changes, aes(x = year, y = pct_change_inst,
                          group = interaction(birth_year, edclass, sex, race),
                          color = factor(birth_year))) +
  geom_line(alpha = 0.5) +
  labs(title = "Percent Change in Institutionalized Population by Group",
       x = "Year", y = "% Change from Previous Year") +
  theme_minimal()

ggplot(group_changes, aes(x = year, y = pct_change_non_inst,
                          group = interaction(birth_year, edclass, sex, race),
                          color = factor(birth_year))) +
  geom_line(alpha = 0.5) +
  labs(title = "Percent Change in Non-Institutionalized Population by Group",
       x = "Year", y = "% Change from Previous Year") +
  theme_minimal()

# Apply log transformation for stationarity checks
log_inst <- log(yearly_totals$total_inst + 1)
log_non_inst <- log(yearly_totals$total_non_inst + 1)

# ADF test for stationarity (inst)
adf_inst <- ur.df(log_inst, type = "drift", lags = 1)
cat("ADF test for log(inst):\n")
print(summary(adf_inst))

# ADF test for stationarity (non_inst)
adf_non_inst <- ur.df(log_non_inst, type = "drift", lags = 1)
cat("\nADF test for log(non_inst):\n")
print(summary(adf_non_inst))

# ACF and PACF plots
par(mfrow = c(2, 2))
acf(diff(log_inst), main = "ACF: Δlog(inst)")
pacf(diff(log_inst), main = "PACF: Δlog(inst)")
acf(diff(log_non_inst), main = "ACF: Δlog(non_inst)")
pacf(diff(log_non_inst), main = "PACF: Δlog(non_inst)")
par(mfrow = c(1, 1))

# Optional: Check patterns by birth cohort
cohort_summary <- data %>%
  group_by(year, birth_year) %>%
  summarise(
    inst = sum(inst, na.rm = TRUE),
    non_inst = sum(non_inst, na.rm = TRUE),
    .groups = "drop"
  )

# Plot institutionalized trends by cohort
ggplot(cohort_summary, aes(x = year, y = inst, group = birth_year, color = factor(birth_year))) +
  geom_line(alpha = 0.5) +
  labs(title = "Institutionalization Trends by Cohort", y = "Institutionalized Count", x = "Year") +
  theme_minimal()

# Plot non-institutionalized trends by cohort
ggplot(cohort_summary, aes(x = year, y = non_inst, group = birth_year, color = factor(birth_year))) +
  geom_line(alpha = 0.5) +
  labs(title = "Non-Institutionalization Trends by Cohort", y = "Non-Institutionalized Count", x = "Year") +
  theme_minimal()