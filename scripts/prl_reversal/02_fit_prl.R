#!/usr/bin/env Rscript

# Fit PRL one‑learning‑rate model (food & neutral conditions)
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Date: 2025‑04‑16

suppressPackageStartupMessages({
  library(fs)
  library(here)
  library(rio)
  library(cmdstanr)
  library(qs)
  library(posterior)
  library(tidyverse)
})

# Create output directories
fs::dir_create(here("scripts", "prl_reversal", "fits"))
fs::dir_create(here("scripts", "prl_reversal", "figures"))

# Compile the Stan model
mod <- cmdstan_model(
  here("scripts", "prl_reversal", "stan", "prl_one_learning_rate.stan")
)

# Function to process a condition ('food' or 'neutral')
process_condition <- function(condition, input_file) {
  message("--- Processing ", condition, " condition ---")
  
  # Load stan data
  stan_data <- rio::import(here("scripts", "prl_reversal", "stan_input_data", input_file))
  G <- stan_data$G
  S <- stan_data$S
  
  # Initializer capturing G and S
  init_fun <- function() {
    list(
      baseline_alpha_raw   = rep(0, G),
      delta_alpha_raw      = rep(0, G),
      baseline_beta_raw    = rep(0, G),
      delta_beta_raw       = rep(0, G),
      sigma_baseline_alpha = rep(0.2, G),
      sigma_delta_alpha    = rep(0.2, G),
      sigma_baseline_beta  = rep(0.2, G),
      sigma_delta_beta     = rep(0.2, G),
      z_baseline_alpha     = rep(0, S),
      z_delta_alpha        = rep(0, S),
      z_baseline_beta      = rep(0, S),
      z_delta_beta         = rep(0, S)
    )
  }
  
  # Sample
  fit <- mod$sample(
    data            = stan_data,
    seed            = 123,
    init            = init_fun,
    iter_warmup     = 1000,
    iter_sampling   = 1000,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 100,
    adapt_delta     = 0.9,
    max_treedepth   = 12
  )
  
  # Save fit
  fit_file <- here("scripts", "prl_reversal", "fits", paste0("fit_", condition, ".qs"))
  qs::qsave(fit, fit_file)
  
  # Reload for extraction
  fit <- qs::qread(fit_file)
  
  # Extract draws and post‑processing
  param_names <- c("baseline_alpha", "delta_alpha", "baseline_beta", "delta_beta", "post_alpha", "post_beta")
  draws_df     <- as_draws_df(fit$draws(variables = param_names))
  
  # Transform & label
  for (i in seq_len(G)) {
    draws_df <- draws_df %>%
      mutate(
        !!paste0("alpha_pre_G", i)  := .data[[paste0("baseline_alpha[", i, "]")]],
        !!paste0("alpha_post_G", i) := .data[[paste0("post_alpha[", i, "]")]],
        !!paste0("beta_pre_G", i)   := .data[[paste0("baseline_beta[", i, "]")]],
        !!paste0("beta_post_G", i)  := .data[[paste0("post_beta[", i, "]")]]
      )
  }
  
  # Summarize
  derived_cols <- purrr::map(c("alpha_pre_", "alpha_post_", "beta_pre_", "beta_post_"),
                             ~grep(.x, names(draws_df), value = TRUE)) %>% unlist()
  summaries <- draws_df %>%
    select(all_of(derived_cols)) %>%
    summarize_draws(
      mean,
      ~quantile2(.x, probs = c(0.025, 0.975))
    )
  print(summaries, n = 40)
  
  # Pre/Post comparison plots
  alpha_df <- summaries %>% filter(str_detect(variable, "alpha_")) %>%
    mutate(
      phase     = if_else(str_detect(variable, "pre"), "pre", "post"),
      group     = case_when(
        str_detect(variable, "_G1") ~ "AN",
        str_detect(variable, "_G2") ~ "HC",
        str_detect(variable, "_G3") ~ "RI"
      )
    )
  p_alpha <- ggplot(alpha_df, aes(group, mean, shape = phase)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5), position = position_dodge(0.3), width = 0.2) +
    labs(title = paste("Alpha (", condition, ") Pre vs Post", sep = ""), x = "Group", y = expression(alpha)) +
    theme_minimal()
  
  beta_df <- summaries %>% filter(str_detect(variable, "beta_")) %>%
    mutate(
      phase     = if_else(str_detect(variable, "pre"), "pre", "post"),
      group     = case_when(
        str_detect(variable, "_G1") ~ "AN",
        str_detect(variable, "_G2") ~ "HC",
        str_detect(variable, "_G3") ~ "RI"
      )
    )
  p_beta <- ggplot(beta_df, aes(group, mean, shape = phase)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5), position = position_dodge(0.3), width = 0.2) +
    labs(title = paste("Beta (", condition, ") Pre vs Post", sep = ""), x = "Group", y = expression(beta)) +
    theme_minimal()
  
  # Save comparison plots
  ggsave(here("scripts", "prl_reversal", "figures", paste0("alpha_pre_post_", condition, ".pdf")), p_alpha, width = 6, height = 4)
  ggsave(here("scripts", "prl_reversal", "figures", paste0("beta_pre_post_", condition, ".pdf")), p_beta, width = 6, height = 4)
  
  # Density plots
  for (pm in c("alpha_pre", "alpha_post", "beta_pre", "beta_post")) {
    data <- tibble(
      value = unlist(draws_df[paste0(pm, c("_G1", "_G2", "_G3"))]),
      Group = rep(c("AN", "HC", "RI"), each = nrow(draws_df))
    )
    p <- ggplot(data, aes(x = value, fill = Group)) +
      geom_density(alpha = 0.5) +
      labs(title = paste(pm, "density (", condition, ")", sep = ""), x = "Value", y = "Density") +
      theme_minimal()
    ggsave(here("scripts", "prl_reversal", "figures", paste0(pm, "_density_", condition, ".pdf")), p, width = 6, height = 4)
  }
  
  # Probability comparisons
  prob_results <- tibble(parameter = character(), comparison = character(), probability = double())
  for (param in c("alpha_pre", "alpha_post", "beta_pre", "beta_post")) {
    g1 <- draws_df[[paste0(param, "_G1")]]
    g2 <- draws_df[[paste0(param, "_G2")]]
    g3 <- draws_df[[paste0(param, "_G3")]]
    prob_results <- bind_rows(
      prob_results,
      tibble(parameter = param, comparison = "G1 < G2", probability = mean(g1 < g2)),
      tibble(parameter = param, comparison = "G1 < G3", probability = mean(g1 < g3))
    )
  }
  prob_results <- prob_results %>% mutate(formatted_prob = paste0(round(probability * 100, 1), "%"))
  print(prob_results, n = nrow(prob_results))
}

# Run both conditions
process_condition("food",    "stan_data_food.RDS")
process_condition("neutral", "stan_data_neutral.RDS")
