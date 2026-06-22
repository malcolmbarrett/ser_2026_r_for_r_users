birthweight_plot <- ggplot(plot_data, aes(x = smoker, y = birth_weight, fill = smoker)) +
  geom_boxplot() +
  labs(
    title = "Birth weight by smoking status",
    x = "Smoker",
    y = "Birth weight (g)"
  ) +
  theme(legend.position = "none")

table1 <- births |>
  select(smoker, mat_age, mat_bmi, parity, education, low_bw, preterm) |>
  tbl_summary(by = smoker) |>
  add_overall()

write_csv(births, here::here("data/births_clean.csv"))

print(birthweight_plot)
print(table1)
print(low_bw_or_table)
print(preterm_or_table)
