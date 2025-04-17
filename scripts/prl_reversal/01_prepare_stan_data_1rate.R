#!/usr/bin/env Rscript

# Generate input data for Stan.
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Date: 2025‑04‑17

# Load necessary libraries ------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(tidybayes)
  library(tidyverse) # To pivot the data
  library(ggdist)
  library(modelr)
  library(loo)
  library(brms)
  options(brms.backend = "cmdstanr", mc.cores = 8)
  library(rstan)
  library(cmdstanr) # Lightweight Stan interface
  library(posterior)
  options(posterior.num_args = list(digits = 2))
  library(bayesplot)
  library(pillar)
  options(pillar.negative = FALSE)
  library(tinytable)
  options(
    tinytable_format_num_fmt = "significant_cell",
    tinytable_format_digits = 2,
    tinytable_tt_digits = 2
  )
  library(matrixStats)
  library(purrr) # List manipulation
  library(extraDistr) # More distributions
  library(MASS) # Tu use the multinormal distribution
  library(mice)
  library(qs) # Functions for quickly writing and reading any R object to and from disk
  library(car)
  library(performance)
  library(effectsize)
  library(missRanger)
  library(RColorBrewer)
  set1 <- RColorBrewer::brewer.pal(3, "Set1")
  library(patchwork)
  library(dslabs)
  ds_theme_set()
})

par(
  family = "serif",
  las = 1,
  bty = "l",
  cex.axis = 1,
  cex.lab = 1,
  cex.main = 1,
  xaxs = "i",
  yaxs = "i",
  mar = c(5, 5, 3, 1)
)


# Import raw PRL data -----------------------------------------------------
df <- rio::import(
  here::here("data", "raw", "ed_prl_data.csv")
)

participants_names <- unique(df$subj_code)

rio::export(
  participants_names,
  here("data", "raw", "participants_names.RDS")
)


# 1) Create a data frame for the "food" condition, 80 trials max ----------
df_food <- df %>%
  dplyr::filter(stim == "food", trial >= 1, trial <= 80) %>%
  mutate(
    # 2) Create "epoch" indicating pre (1..40) vs. post (41..80)
    epoch = if_else(trial <= 40, 1L, 2L),

    # 3) Create a "clean" subject index
    subj_new = as.integer(factor(subj_idx))
  )

# 4) Build the table mapping each subject to its group index
subj_group_food <- df_food %>%
  distinct(subj_new, diag_cat) %>%
  mutate(
    group_idx = case_when(
      diag_cat == "AN" ~ 1L,
      diag_cat == "HC" ~ 2L,
      diag_cat == "RI" ~ 3L,
      TRUE ~ NA_integer_
    )
  ) %>%
  arrange(subj_new)

# 5) Get the counts
S_food <- n_distinct(df_food$subj_new) # number of subjects (food)
N_food <- nrow(df_food) # number of total trials (food)
G <- 3 # total groups (AN=1, HC=2, RI=3)

# 6) Build the vectors for Stan
subj_food <- df_food$subj_new
group_food <- subj_group_food$group_idx
choice_food <- df_food$response # 0/1
feedback_food <- df_food$feedback # 0/1
epoch_food <- df_food$epoch # 1=pre, 2=post

# 7) Assemble the Stan data list for the 'food' condition
stan_data_food <- list(
  S = S_food,
  G = G,
  N = N_food,
  subj = subj_food,
  group = group_food,
  choice = choice_food,
  feedback = feedback_food,
  epoch = epoch_food
)

# Repeat the same process for the "neutral" condition ---------------------
df_neutral <- df %>%
  dplyr::filter(stim == "neutral", trial >= 1, trial <= 80) %>%
  mutate(
    epoch = if_else(trial <= 40, 1L, 2L),
    subj_new = as.integer(factor(subj_idx))
  )

subj_group_neutral <- df_neutral %>%
  distinct(subj_new, diag_cat) %>%
  mutate(
    group_idx = case_when(
      diag_cat == "AN" ~ 1L,
      diag_cat == "HC" ~ 2L,
      diag_cat == "RI" ~ 3L
    )
  ) %>%
  arrange(subj_new)

S_neutral <- n_distinct(df_neutral$subj_new)
N_neutral <- nrow(df_neutral)

subj_neutral <- df_neutral$subj_new
group_neutral <- subj_group_neutral$group_idx
choice_neutral <- df_neutral$response
feedback_neutral <- df_neutral$feedback
epoch_neutral <- df_neutral$epoch

stan_data_neutral <- list(
  S = S_neutral,
  G = G,
  N = N_neutral,
  subj = subj_neutral,
  group = group_neutral,
  choice = choice_neutral,
  feedback = feedback_neutral,
  epoch = epoch_neutral
)

# Save Stan input lists as RDS
rio::export(
  stan_data_food,
  here("scripts", "prl_reversal", "stan_input_data", "stan_data_food.RDS")
)

rio::export(
  stan_data_neutral,
  here("scripts", "prl_reversal", "stan_input_data", "stan_data_neutral.RDS")
)

# eof ---
