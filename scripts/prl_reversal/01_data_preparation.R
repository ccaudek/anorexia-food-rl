# Overview ----------------------------------------------------------------
# Associated project: Groundhog Day PRL
# Script purpose: Run a Rescorla-Wagner Stan model that accounts for both 
#  autobiographic and fractal stimuli, and consider the effect of instant
#  mood. 
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: Sun Sep  8 10:36:16 CEST 2024
# Last update: Sun Sep  8 10:36:16 CEST 2024
# Status: In progress
# Notes: 


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
  options(posterior.num_args=list(digits=2))
  library(bayesplot)
  library(pillar)
  options(pillar.negative = FALSE)
  library(tinytable)
  options(tinytable_format_num_fmt = "significant_cell", tinytable_format_digits = 2, tinytable_tt_digits=2)
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

par(family="serif", las=1, bty="l",
    cex.axis=1, cex.lab=1, cex.main=1,
    xaxs="i", yaxs="i", mar = c(5, 5, 3, 1)
)


# Import PRL data ---------------------------------------------------------

# Data include both sessions with and without reversal

# Import and clean autobiographical data
df <- rio::import(
  here::here("data", "raw", "ed_prl_data.csv")
) 

# 1) Crea un dataframe con SOLO stim = "food"
df_food <- df %>%
  dplyr::filter(stim == "food", trial >= 1, trial <= 80)

df_food <- df_food %>%
  # 2) Crea variabile epoch: pre/post a seconda del trial
  mutate(
    epoch = if_else(trial <= 40, 1L, 2L)
  ) %>%
  # 3) Crea un indice soggetto "pulito" partendo da subj_idx
  mutate(
    subj_new = as.integer(factor(subj_idx))
  )

# 4) Costruisci la tabella per l'indice di gruppo
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

# 5) Recupera S, N, G
S_food <- n_distinct(df_food$subj_new)
N_food <- nrow(df_food)
G <- 3  # i gruppi totali (AN, HC, RI)

# 6) Vettori da passare a Stan
subj_food    <- df_food$subj_new
group_food   <- subj_group_food$group_idx
choice_food  <- df_food$response   # 0/1 (anche se qui è "food", la colonna indica se lo stim è stato scelto?)
feedback_food<- df_food$feedback
epoch_food   <- df_food$epoch

# 7) Assembla lo stan_data "food"
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

# Ora puoi fare la stessa identica cosa per stim = "neutral"
df_neutral <- df %>%
  dplyr::filter(stim == "neutral", trial >= 1, trial <= 80) |> 
  mutate(epoch = if_else(trial <= 40, 1L, 2L),
         subj_new = as.integer(factor(subj_idx)))

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

subj_neutral    <- df_neutral$subj_new
group_neutral   <- subj_group_neutral$group_idx
choice_neutral  <- df_neutral$response
feedback_neutral<- df_neutral$feedback
epoch_neutral   <- df_neutral$epoch

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


# eof ---