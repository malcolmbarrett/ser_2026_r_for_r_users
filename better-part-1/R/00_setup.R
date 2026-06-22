library(tidyverse)
library(janitor)
library(broom)
library(gtsummary)
library(here)

age_min <- 18
bad_ids <- 1:3
bw_cutoff <- 2500
preterm_cutoff <- 37
complete_outcome <- TRUE
