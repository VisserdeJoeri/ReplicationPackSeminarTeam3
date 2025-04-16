library(dplyr)
library(purrr)
library(tidyr)

combined_rates <- combined_acs_cps %>%
  filter(year >= 2001, year <= 2018) %>%
  mutate(
    birth_year = year - age,
    total_pop = inst + non_inst,
    inst_rate = ifelse(total_pop > 0, inst / total_pop, NA)
  ) %>%
  filter(!is.na(inst_rate))

group_models <- combined_rates %>%
  group_by(birth_year, edclass, sex, race) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(inst_rate ~ year, data = .x))
  )

future_years <- expand_grid(
  year = 2019:2021,
  birth_year = unique(combined_rates$birth_year),
  edclass = unique(combined_rates$edclass),
  sex = unique(combined_rates$sex),
  race = unique(combined_rates$race)
) %>%
  mutate(age = year - birth_year)

future_rates <- group_models %>%
  select(birth_year, edclass, sex, race, model) %>%
  right_join(future_years, by = c("birth_year", "edclass", "sex", "race")) %>%
  mutate(
    predicted_rate = map2_dbl(model, year, ~ {
      if (is.null(.x)) return(NA)
      predict(.x, newdata = data.frame(year = .y))
    })
  )

library(dplyr)

# Step 1–2: Add birth year and cohort block
cohort_data <- combined_acs_cps %>%
  mutate(
    birth_year = year - age,
    birth_cohort = paste0(
      floor(birth_year / 5) * 5, "-",
      floor(birth_year / 5) * 5 + 4
    )
  )

library(dplyr)

# Create 5-year birth cohort and aggregate
library(dplyr)

library(dplyr)

library(dplyr)
library(ggplot2)

combined_acs_cps <- combined_acs_cps %>%
  mutate(
    age_group = cut(
      age,
      breaks = seq(25, 70, by = 5),
      right = FALSE,
      labels = c("25–29", "30–34", "35–39", "40–44", "45–49",
                 "50–54", "55–59", "60–64", "65–69")
    )
  )

library(dplyr)

library(dplyr)
library(ggplot2)

# Step 1: Create age group variable
combined_acs_cps <- combined_acs_cps %>%
  mutate(
    age_group = cut(
      age,
      breaks = seq(25, 70, by = 5),
      right = FALSE,
      labels = c("25–29", "30–34", "35–39", "40–44", "45–49",
                 "50–54", "55–59", "60–64", "65–69")
    )
  )

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# Step 1: Add age group, edclass label, and sex label
combined_acs_cps <- combined_acs_cps %>%
  mutate(
    age_group = cut(
      age,
      breaks = seq(25, 70, by = 5),
      right = FALSE,
      labels = c("25–29", "30–34", "35–39", "40–44", "45–49",
                 "50–54", "55–59", "60–64", "65–69")
    ),
    edclass_label = case_when(
      edclass == 1 ~ "Less than High School",
      edclass == 2 ~ "High School Graduate",
      edclass == 3 ~ "Some College",
      edclass == 4 ~ "College Graduate",
      TRUE ~ NA_character_
    ),
    sex_label = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

# Step 2: Aggregate by age group, edclass, sex, and year
age_edclass_sex_grouped <- combined_acs_cps %>%
  filter(!is.na(age_group), !is.na(edclass_label), !is.na(sex_label)) %>%
  group_by(age_group, edclass_label, sex_label, year) %>%
  summarise(
    inst = sum(inst, na.rm = TRUE),
    non_inst = sum(non_inst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total = inst + non_inst,
    inst_rate = ifelse(total > 0, inst / total, NA),
    edclass_label = factor(edclass_label, levels = c(
      "Less than High School", "High School Graduate", 
      "Some College", "College Graduate"
    )),
    sex_label = factor(sex_label, levels = c("Male", "Female"))
  )

# Step 3: Plot smoothed institutionalization rate
ggplot(age_edclass_sex_grouped %>% filter(!is.na(inst_rate)),
       aes(x = year, y = inst_rate, color = age_group)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4, linewidth = 1.1) +
  facet_grid(sex_label ~ edclass_label) +
  labs(
    title = "Smoothed Institutionalization Rate by Age, Education, and Sex",
    subtitle = "Colored by age group; faceted by education class (columns) and sex (rows)",
    x = "Year",
    y = "Institutionalization Rate",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 11)
  )