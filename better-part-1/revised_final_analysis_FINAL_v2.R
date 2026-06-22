####### smoking & birth weight analysis ########

library(tidyverse)
library(janitor)
library(broom)
library(gtsummary)

births <- read.csv(here::here("data/births.csv"))
births <- clean_names(births)

# restrict to adult moms only
age_min <- 18
births <- births[births$mat_age >= age_min, ]

# drop the ones flagged at the data meeting
bad_ids <- 1:3
births <- births[!births$id %in% bad_ids, ]

# quick look at the data
mean(births$birth_weight)
table(births$smoker)
head(births, n = 20)


########## FIGURE FOR THE GRANT ###########
# (PI wanted this for the renewal, looks nice)

# data for the figure above
plot_data <- births

ggplot(plot_data, aes(x = smoker, y = birth_weight, fill = smoker)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Birth weight by smoking status",
    x = "Smoker",
    y = "Birth weight (g)"
  ) +
  theme(legend.position = "none")


# make the outcomes
bw_cutoff <- 2500
preterm_cutoff <- 37
births$low_bw <- ifelse(births$birth_weight < bw_cutoff, 1, 0)
births$preterm <- ifelse(births$ges_age < preterm_cutoff, 1, 0)
births$smoke <- ifelse(births$smoker == "Yes", 1, 0)

complete_outcome <- TRUE
if (complete_outcome) {
  births <- births[!is.na(births$birth_weight), ]
}


######## TABLE 1 NUMBERS (for Table1.docx) ########
table1 <- births |>
    select(smoker, mat_age, mat_bmi, parity, education, low_bw, preterm) |>
    tbl_summary(by = smoker) |>
    add_overall()
table1 # copy and paste into Word doc


######## THE MODEL ########
model1 <- glm(
  low_bw ~ smoke + mat_age + mat_bmi + as.factor(education) + parity,
  data = births,
  family = binomial
)
summary(model1)

# odds ratios -- using broom::tidy()
tidy(model1, exponentiate = TRUE)


######## preterm model ########
model2 = glm(
  preterm ~ smoke + mat_age + mat_bmi + as.factor(education) + parity,
  data = births,
  family = binomial
)
summary(model2)
tidy(model2, exponentiate = TRUE)


# save cleaned data so i don't have to rerun all this
write.csv(births, here::here("data/births_clean.csv"))

# TODO: sensitivity analysis
