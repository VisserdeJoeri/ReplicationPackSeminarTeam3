# This code creates the graph for the Robustness chapter

library(dplyr)
library(ggplot2)

# Create lagged inst_rate from year t-1, only for races 1 & 2
lagged_rates <- combined_dataset %>%
  filter(year %in% 2000:2018, race %in% c(1, 2)) %>%
  select(year, race, sex, age, edclass, inst_rate_lag = inst_rate) %>%
  mutate(year = year + 1)

# Step 2: Join and compute constructed vs. actual total population
total_diff_data <- combined_dataset %>%
  filter(year %in% 2001:2018, race %in% c(1, 2)) %>%
  left_join(lagged_rates, by = c("year", "race", "sex", "age", "edclass")) %>%
  mutate(
    inst_hat = inst_rate_lag * (inst.x + non_inst),
    total_actual = inst.x + non_inst,
    total_constructed = inst_hat + non_inst,
    perc_diff_total = 100 * (total_constructed - total_actual) / total_actual
  )

# Step 3: Plot percentage difference
ggplot(total_diff_data, aes(x = factor(year), y = perc_diff_total)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Percentual Difference in Total Population: Constructed vs Actual (Race 1 & 2)",
    subtitle = "Constructed uses inst_rate from year t-1",
    x = "Year",
    y = "% Difference"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))