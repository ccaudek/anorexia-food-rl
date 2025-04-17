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

Sys.setenv(CXXFLAGS = "-O3 -march=native -mtune=native")

# Create output directories
fs::dir_create(here("scripts", "prl_reversal", "fits"))
fs::dir_create(here("scripts", "prl_reversal", "figures"))

# Compile the Stan model
mod <- cmdstan_model(
  here("scripts", "prl_reversal", "stan", "prl_one_learning_rate.stan"),
  cpp_options = list(stan_threads = TRUE)
)

# Function to process a condition ('food' or 'neutral')
process_condition <- function(condition, input_file) {
  message("--- Processing ", condition, " condition ---")

  # Load stan data
  stan_data <- rio::import(here(
    "scripts",
    "prl_reversal",
    "stan_input_data",
    input_file
  ))
  G <- stan_data$G
  S <- stan_data$S

  # Initializer capturing G and S
  init_fun <- function() {
    list(
      baseline_alpha_raw = rep(0, G),
      delta_alpha_raw = rep(0, G),
      baseline_beta_raw = rep(0, G),
      delta_beta_raw = rep(0, G),
      sigma_baseline_alpha = rep(0.2, G),
      sigma_delta_alpha = rep(0.2, G),
      sigma_baseline_beta = rep(0.2, G),
      sigma_delta_beta = rep(0.2, G),
      z_baseline_alpha = rep(0, S),
      z_delta_alpha = rep(0, S),
      z_baseline_beta = rep(0, S),
      z_delta_beta = rep(0, S)
    )
  }

  # Sample
  fit <- mod$sample(
    data = stan_data,
    seed = 123,
    init = init_fun,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 2, # 4 chains × 2 threads = 8 logical cores
    refresh = 100,
    adapt_delta = 0.9,
    max_treedepth = 12
  )

  # Save fit
  fit_file <- here(
    "scripts",
    "prl_reversal",
    "fits",
    paste0("fit_", condition, ".qs")
  )
  qs::qsave(fit, fit_file)

  # Reload for extraction
  fit <- qs::qread(fit_file)

  # Extract draws and post‑processing
  param_names <- c(
    "baseline_alpha",
    "delta_alpha",
    "baseline_beta",
    "delta_beta",
    "post_alpha",
    "post_beta"
  )
  draws_df <- as_draws_df(fit$draws(variables = param_names))

  # Transform & label
  for (i in seq_len(G)) {
    draws_df <- draws_df %>%
      mutate(
        !!paste0("alpha_pre_G", i) := .data[[paste0(
          "baseline_alpha[",
          i,
          "]"
        )]],
        !!paste0("alpha_post_G", i) := .data[[paste0("post_alpha[", i, "]")]],
        !!paste0("beta_pre_G", i) := .data[[paste0("baseline_beta[", i, "]")]],
        !!paste0("beta_post_G", i) := .data[[paste0("post_beta[", i, "]")]]
      )
  }

  # Summarize
  derived_cols <- purrr::map(
    c("alpha_pre_", "alpha_post_", "beta_pre_", "beta_post_"),
    ~ grep(.x, names(draws_df), value = TRUE)
  ) %>%
    unlist()
  summaries <- draws_df %>%
    select(all_of(derived_cols)) %>%
    summarize_draws(
      mean,
      ~ quantile2(.x, probs = c(0.025, 0.975))
    )
  print(summaries, n = 40)

  # Pre/Post comparison plots
  alpha_df <- summaries %>%
    filter(str_detect(variable, "alpha_")) %>%
    mutate(
      phase = if_else(str_detect(variable, "pre"), "pre", "post"),
      group = case_when(
        str_detect(variable, "_G1") ~ "AN",
        str_detect(variable, "_G2") ~ "HC",
        str_detect(variable, "_G3") ~ "RI"
      )
    )
  p_alpha <- ggplot(alpha_df, aes(group, mean, shape = phase)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(
      aes(ymin = q2.5, ymax = q97.5),
      position = position_dodge(0.3),
      width = 0.2
    ) +
    labs(
      title = paste("Alpha (", condition, ") Pre vs Post", sep = ""),
      x = "Group",
      y = expression(alpha)
    ) +
    theme_minimal()

  beta_df <- summaries %>%
    filter(str_detect(variable, "beta_")) %>%
    mutate(
      phase = if_else(str_detect(variable, "pre"), "pre", "post"),
      group = case_when(
        str_detect(variable, "_G1") ~ "AN",
        str_detect(variable, "_G2") ~ "HC",
        str_detect(variable, "_G3") ~ "RI"
      )
    )
  p_beta <- ggplot(beta_df, aes(group, mean, shape = phase)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(
      aes(ymin = q2.5, ymax = q97.5),
      position = position_dodge(0.3),
      width = 0.2
    ) +
    labs(
      title = paste("Beta (", condition, ") Pre vs Post", sep = ""),
      x = "Group",
      y = expression(beta)
    ) +
    theme_minimal()

  # Save comparison plots
  ggsave(
    here(
      "scripts",
      "prl_reversal",
      "figures",
      paste0("alpha_pre_post_", condition, ".pdf")
    ),
    p_alpha,
    width = 6,
    height = 4
  )
  ggsave(
    here(
      "scripts",
      "prl_reversal",
      "figures",
      paste0("beta_pre_post_", condition, ".pdf")
    ),
    p_beta,
    width = 6,
    height = 4
  )

  # Density plots
  for (pm in c("alpha_pre", "alpha_post", "beta_pre", "beta_post")) {
    data <- tibble(
      value = unlist(draws_df[paste0(pm, c("_G1", "_G2", "_G3"))]),
      Group = rep(c("AN", "HC", "RI"), each = nrow(draws_df))
    )
    p <- ggplot(data, aes(x = value, fill = Group)) +
      geom_density(alpha = 0.5) +
      labs(
        title = paste(pm, "density (", condition, ")", sep = ""),
        x = "Value",
        y = "Density"
      ) +
      theme_minimal()
    ggsave(
      here(
        "scripts",
        "prl_reversal",
        "figures",
        paste0(pm, "_density_", condition, ".pdf")
      ),
      p,
      width = 6,
      height = 4
    )
  }

  # Probability comparisons
  prob_results <- tibble(
    parameter = character(),
    comparison = character(),
    probability = double()
  )
  for (param in c("alpha_pre", "alpha_post", "beta_pre", "beta_post")) {
    g1 <- draws_df[[paste0(param, "_G1")]]
    g2 <- draws_df[[paste0(param, "_G2")]]
    g3 <- draws_df[[paste0(param, "_G3")]]
    prob_results <- bind_rows(
      prob_results,
      tibble(
        parameter = param,
        comparison = "G1 < G2",
        probability = mean(g1 < g2)
      ),
      tibble(
        parameter = param,
        comparison = "G1 < G3",
        probability = mean(g1 < g3)
      )
    )
  }
  prob_results <- prob_results %>%
    mutate(formatted_prob = paste0(round(probability * 100, 1), "%"))
  print(prob_results, n = nrow(prob_results))
}

# Run both conditions
process_condition("food", "stan_data_food.RDS")
process_condition("neutral", "stan_data_neutral.RDS")

# G1: AN, G2: HC, G3: RI
#
# FOOD
# # A tibble: 12 × 4
# variable       mean  q2.5 q97.5
# <chr>         <dbl> <dbl> <dbl>
# 1 alpha_pre_G1  0.550 0.304 0.779
# 2 alpha_pre_G2  0.835 0.695 0.932
# 3 alpha_pre_G3  0.896 0.806 0.961
# 4 alpha_post_G1 0.312 0.170 0.492
# 5 alpha_post_G2 0.734 0.594 0.852
# 6 alpha_post_G3 0.706 0.535 0.849
# 7 beta_pre_G1   1.02  0.581 1.61
# 8 beta_pre_G2   1.16  0.826 1.56
# 9 beta_pre_G3   1.23  0.865 1.66
# 10 beta_post_G1  1.89  1.07  2.98
# 11 beta_post_G2  1.59  1.13  2.15
# 12 beta_post_G3  1.60  0.986 2.34
# # A tibble: 8 × 4
# parameter  comparison probability formatted_prob
# <chr>      <chr>            <dbl> <chr>
# 1 alpha_pre  G1 < G2          0.983 98.3%
# 2 alpha_pre  G1 < G3          0.998 99.8%
# 3 alpha_post G1 < G2          1.00  100%
# 4 alpha_post G1 < G3          0.998 99.8%
# 5 beta_pre   G1 < G2          0.693 69.3%
# 6 beta_pre   G1 < G3          0.752 75.2%
# 7 beta_post  G1 < G2          0.302 30.2%
# 8 beta_post  G1 < G3          0.315 31.5%

# NEUTRAL
# A tibble: 12 × 4
# variable       mean  q2.5 q97.5
# <chr>         <dbl> <dbl> <dbl>
# 1 alpha_pre_G1  0.855 0.761 0.932
# 2 alpha_pre_G2  0.882 0.745 0.960
# 3 alpha_pre_G3  0.815 0.639 0.939
# 4 alpha_post_G1 0.803 0.708 0.883
# 5 alpha_post_G2 0.777 0.658 0.869
# 6 alpha_post_G3 0.757 0.594 0.879
# 7 beta_pre_G1   1.09  0.647 1.62
# 8 beta_pre_G2   1.65  1.17  2.27
# 9 beta_pre_G3   1.77  1.12  2.59
# 10 beta_post_G1  0.847 0.446 1.41
# 11 beta_post_G2  1.41  0.941 1.99
# 12 beta_post_G3  1.64  1.00  2.45
# # A tibble: 8 × 4
# parameter  comparison probability formatted_prob
# <chr>      <chr>            <dbl> <chr>
# 1 alpha_pre  G1 < G2          0.684 68.4%
# 2 alpha_pre  G1 < G3          0.341 34.1%
# 3 alpha_post G1 < G2          0.360 36%
# 4 alpha_post G1 < G3          0.304 30.4%
# 5 beta_pre   G1 < G2          0.941 94.1%
# 6 beta_pre   G1 < G3          0.945 94.5%
# 7 beta_post  G1 < G2          0.941 94.1%
# 8 beta_post  G1 < G3          0.967 96.7%
