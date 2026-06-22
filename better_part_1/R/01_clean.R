births <- read_csv(here::here("data/births.csv")) |>
  clean_names() |>
  filter(
    mat_age >= age_min,
    !id %in% bad_ids
  ) |>
  mutate(
    low_bw = if_else(birth_weight < bw_cutoff, 1, 0),
    preterm = if_else(ges_age < preterm_cutoff, 1, 0),
    smoke = if_else(smoker == "Yes", 1, 0)
  )

if (complete_outcome) {
  births <- births |>
    filter(!is.na(birth_weight))
}

plot_data <- births
