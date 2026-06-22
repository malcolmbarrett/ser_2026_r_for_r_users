low_bw_model <- glm(
  low_bw ~ smoke + mat_age + mat_bmi + as.factor(education) + parity,
  data = births,
  family = binomial
)

preterm_model <- glm(
  preterm ~ smoke + mat_age + mat_bmi + as.factor(education) + parity,
  data = births,
  family = binomial
)

low_bw_or_table <- tidy(
  low_bw_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

preterm_or_table <- tidy(
  preterm_model,
  exponentiate = TRUE,
  conf.int = TRUE
)
