# Spontaneous abortion and infertility: a bootstrap odds ratio.
#
# A colleague sent you this script. They say it "almost works" but they
# cannot get it to run start to finish. Work through each YOUR TURN block
# in order. Each one points you at a debugging tool; use it to understand
# and fix the problem.
#
# The data is `infert`, a built-in case-control study of infertility.

library(ggplot2)
library(purrr)
library(polars) # I did the data prep in polars

# ===== YOUR TURN 1 =====
# The library(polars) call above fails: polars is not installed, and it is
# not on CRAN, so the usual install will not find it.
# 1. Run install.packages("polars") and read the error.
# 2. polars is published on R-multiverse. Install it from that repository: "https://community.r-multiverse.org"
# 3. Re-run library(polars), then carry on.

# Data prep in polars: add the log of the spontaneous-abortion count.
inf_pl <- as_polars_df(infert)$with_columns(
  pl$col("spontaneous")$log()$alias("log_spont")
)
inf <- as.data.frame(inf_pl)

# A quick look at case status against the (logged) number of abortions.
ggplot(inf, aes(log_spont, case)) +
  geom_point() +
  geom_smooth(method = "lm")

# ===== YOUR TURN 2 =====
# That plot prints a warning that is easy to scroll past:
# a lot of rows are being dropped before the line is fit.
# 1. Run old <- options(warn = 2) to turn warnings into errors.
# 2. Re-run the plot. Read the traceback to see what is non-finite.
# 3. Decide how to handle those rows, then set options(old) again
# before continuing.

fit <- glm(case ~ spontaneous + induced, data = inf, family = binomial)
or <- exp(coef(fit))
ci <- exp(confint(fit)) # 95% Wald confidence intervals for the odds ratios
cbind(or, ci)

# ===== YOUR TURN 3 =====
# The comment above says that these are Wald intervals, but you notice that confint() prints "Waiting for profiling to be done...".
# Check the source code of confint.glm() to see what is going on.
# 1. Run debugonce(stats:::confint.glm).
# 2. Re-run confint(fit) and step through. Notice it calls profile().
# 3. Confirm it: check that confint(profile(fit)) returns the same intervals as confint(fit).
# 4. These are profiled-likelihood intervals, not Wald. Fix the comment.

# Subgroup filter in polars: the least-educated women (n = 12).
sub <- as.data.frame(inf_pl$filter(pl$col("education") == "0-5yrs"))

# Bootstrap the spontaneous-abortion odds ratio in this subgroup.
boot_or <- function(i) {
  d <- sub[sample(nrow(sub), replace = TRUE), ]
  fit <- glm(case ~ spontaneous + factor(induced), data = d, family = binomial)
  exp(coef(fit))[["spontaneous"]]
}

set.seed(2026)
ors <- map_dbl(1:2000, boot_or)
quantile(ors, c(0.025, 0.975))

# ===== YOUR TURN 4 =====
# map_dbl() errors partway through. The message reports the failing
# iteration, "In index: N". Use it to pause right there.
# 1. Note the index N from the error message.
# 2. Add  if (i == N) browser()  as the first line of boot_or().
# 3. Re-run; at the pause, check table(d$induced) and nrow(sub).
# 4. The subgroup has only 12 people, so some resamples contain a single
#    induced level
# 5. Bootstrap the full cohort instead. Use `inf` in place of `sub` inside
#    boot_or(), remove the browser() line, and re-run.

# ===== YOUR TURN 5 =====
# To get here, you hand-installed polars from a non-CRAN repository into your
# system library. None of that is recorded, so the next person (or future
# you) cannot reproduce it. Make the project reproducible with renv.
# 1. .libPaths()        # see which library you are installing into now.
# 2. renv::init()       # creates a project-local library and an .Rprofile
#                       #   that loads renv every time the project opens.
# 3. .libPaths()        # confirm it now points inside the project.
# 4. Reinstall polars into the project library, again from R-multiverse:
#    renv::install("polars", repos = "https://community.r-multiverse.org")
#    Install any other packages the script needs the same way.
# 5. renv::snapshot()   # record every package and version in renv.lock.
