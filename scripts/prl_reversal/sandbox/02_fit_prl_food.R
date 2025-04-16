#!/usr/bin/env Rscript

# Fit PRL one‑learning‑rate model (food condition)
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

# Create output directories if they don't exist
fs::dir_create(here::here("scripts", "prl_reversal", "fits"))
fs::dir_create(here::here("scripts", "prl_reversal", "figures"))

# Load Stan data prepared in earlier stage
stan_data_food <- rio::import(
  here::here("scripts", "prl_reversal", "stan_input_data", "stan_data_food.RDS")
)
G      <- stan_data_food$G       # number of groups (AN=1, HC=2, RI=3)
S_food <- stan_data_food$S       # number of subjects (food condition)

# Compile model -----------------------------------------------------------

mod <- cmdstan_model(
  here::here("scripts", "prl_reversal", "stan", "prl_one_learning_rate.stan")
) 
# mod$print()

# Run sampling ------------------------------------------------------------
init_func <- function() {
  list(
    # Group-level parameters in raw space (logit/log)
    baseline_alpha_raw = rep(0, G),  # logit(0.5) = 0
    delta_alpha_raw    = rep(0, G),  # logit(0.5) = 0
    baseline_beta_raw  = rep(0, G),  # log(1) = 0
    delta_beta_raw     = rep(0, G),  # log(1) = 0
    
    # Standard deviations (small positive values)
    sigma_baseline_alpha = rep(0.2, G),
    sigma_delta_alpha    = rep(0.2, G),
    sigma_baseline_beta  = rep(0.2, G),
    sigma_delta_beta     = rep(0.2, G),
    
    # Z-scores (standard normal) for S subjects
    z_baseline_alpha = rep(0, S_food),
    z_delta_alpha    = rep(0, S_food),
    z_baseline_beta  = rep(0, S_food),
    z_delta_beta     = rep(0, S_food)
  )
}

fit_food <- mod$sample(
  data = stan_data_food,
  seed = 123,
  init = rep(list(init_func()), 4),  # One list per chain
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  adapt_delta = 0.9,  # Increase adapt_delta to reduce step size
  max_treedepth = 12  # Increase max_treedepth to allow more exploration
)

# Save the fitted object -------------------------------------------------
qs::qsave(
  x = fit_food,
  file = here::here("scripts", "prl_reversal", "fits", "fit_food.qs")
)

# Reload fit (optional) ---------------------------------------------------
fit_food <- qs::qread(
 here::here("scripts", "prl_reversal", "fits", "fit_food.qs")
)


# Extract posterior draws for group‑level parameters ----------------------
param_names  <- c(
  "baseline_alpha", "delta_alpha", "baseline_beta", "delta_beta", 
  "post_alpha", "post_beta"
)
draws_array  <- fit_food$draws(variables = param_names)
# Convert to data frame
draws_df     <- as_draws_df(draws_array)


# Transform and label parameters ------------------------------------------
for (i in seq_len(G)) {
  draws_df <- draws_df %>%
    dplyr::mutate(
      # alpha_pre_Gi
      !!paste0("alpha_pre_G", i)  := .data[[paste0("baseline_alpha[", i, "]")]],
      !!paste0("alpha_post_G", i) := .data[[paste0("post_alpha[", i, "]")]],
      
      # beta_pre_Gi
      !!paste0("beta_pre_G", i)   := .data[[paste0("baseline_beta[", i, "]")]],
      !!paste0("beta_post_G", i)  := .data[[paste0("post_beta[", i, "]")]]
    )
}


# Summarize with posterior means & 95% CIs -------------------------------
param_prefixes <- c("alpha_pre_", "alpha_post_", "beta_pre_", "beta_post_")
# Grab matching columns
derived_cols <- 
  purrr::map(param_prefixes, ~grep(.x, names(draws_df), value=TRUE)) %>%
  unlist()

summaries <- draws_df %>%
  dplyr::select(all_of(derived_cols)) %>%
  summarize_draws(
    mean,
    ~quantile2(.x, probs = c(0.025, 0.975))
  )

print(summaries, n=40)


# Plot pre/post comparisons ----------------------------------------------

# a) Alpha
alpha_df <- summaries %>%
  filter(grepl("alpha_", variable)) %>%
  mutate(
    phase = ifelse(grepl("pre", variable), "pre", "post"),
    group_num = sub(".*_G", "", variable),
    group = case_when(
      group_num == "1" ~ "AN",
      group_num == "2" ~ "HC",
      group_num == "3" ~ "RI",
      TRUE ~ group_num
    )
  )

p_alpha <- ggplot(alpha_df, aes(x = group, y = mean, shape = phase)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = q2.5, ymax = q97.5),
    position = position_dodge(width = 0.3),
    width = 0.2
  ) +
  labs(title = "Alpha Parameter (Pre vs. Post)",
       x = "Group",
       y = expression(alpha)) +
  theme_minimal()

p_alpha

# b) Beta
beta_df <- summaries %>%
  filter(grepl("beta_", variable)) %>%
  mutate(
    phase = ifelse(grepl("pre", variable), "pre", "post"),
    group_num = sub(".*_G", "", variable),
    group = case_when(
      group_num == "1" ~ "AN",
      group_num == "2" ~ "HC",
      group_num == "3" ~ "RI",
      TRUE ~ group_num
    )
  )

p_beta <- ggplot(beta_df, aes(x = group, y = mean, shape = phase)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = q2.5, ymax = q97.5),
    position = position_dodge(width = 0.3),
    width = 0.2
  ) +
  labs(title = "Temperature Parameter (Pre vs. Post)",
       x = "Group",
       y = expression(beta)) +
  theme_minimal()

p_beta

# Save comparison plots --------------------------------------------------
ggsave(
  filename = here::here(
    "scripts", "prl_reversal", "figures", "alpha_pre_post_food.pdf"),
  plot = p_alpha, width = 6, height = 4
)

ggsave(
  filename = here::here(
    "scripts", "prl_reversal", "figures", "beta_pre_post_food.pdf"),
  plot = p_beta, width = 6, height = 4
)


# Density‐plot function and saving ---------------------------------------
plot_parameter_densities <- function(param_name) {
  param_data <- tibble(
    value = c(
      draws_df[[paste0(param_name, "_G1")]],
      draws_df[[paste0(param_name, "_G2")]],
      draws_df[[paste0(param_name, "_G3")]]
    ),
    Group = factor(rep(c("Group 1 (AN)", "Group 2 (HC)", "Group 3 (RI)"),
                       each = nrow(draws_df)))
  )
  ggplot(param_data, aes(x = value, fill = Group)) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste("Posterior Distributions for", param_name),
      x     = "Parameter Value",
      y     = "Density"
    ) +
    theme_minimal()
}

for (pm in c("alpha_pre", "alpha_post", "beta_pre", "beta_post")) {
  p <- plot_parameter_densities(pm)
  ggsave(
    filename = here::here("scripts", "prl_reversal", "figures", paste0(pm, "_density_food.pdf")),
    plot     = p, width = 6, height = 4
  )
}

# Probability comparisons ------------------------------------------------
parameters  <- c("alpha_pre", "alpha_post", "beta_pre", "beta_post")
prob_results <- tibble(parameter = character(), comparison = character(), probability = double())
for (param in parameters) {
  g1 <- draws_df[[paste0(param, "_G1")]]
  g2 <- draws_df[[paste0(param, "_G2")]]
  g3 <- draws_df[[paste0(param, "_G3")]]
  prob_results <- bind_rows(
    prob_results,
    tibble(parameter = param, comparison = "G1 < G2", probability = mean(g1 < g2)),
    tibble(parameter = param, comparison = "G1 < G3", probability = mean(g1 < g3))
  )
}
prob_results <- prob_results %>%
  mutate(formatted_prob = paste0(round(probability * 100, 1), "%"))
print(prob_results, n = nrow(prob_results))


# 
# parameters <- c("alpha_pre", "alpha_post", "beta_pre", "beta_post")
# 
# prob_results <- data.frame(
#   parameter = character(),
#   comparison = character(),
#   probability = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for (param in parameters) {
#   # e.g. draws_df$alpha_pre_G1, draws_df$alpha_pre_G2
#   g1 <- draws_df[[paste0(param, "_G1")]]
#   g2 <- draws_df[[paste0(param, "_G2")]]
#   g3 <- draws_df[[paste0(param, "_G3")]]
#   
#   prob_g1_lt_g2 <- mean(g1 < g2)
#   prob_g1_lt_g3 <- mean(g1 < g3)
#   
#   prob_results <- rbind(
#     prob_results,
#     data.frame(parameter = param, comparison = "G1 < G2", probability = prob_g1_lt_g2),
#     data.frame(parameter = param, comparison = "G1 < G3", probability = prob_g1_lt_g3)
#   )
# }
# 
# prob_results$formatted_prob <- paste0(round(prob_results$probability * 100, 1), "%")
# print(prob_results, row.names = FALSE)
# 
# 
# # -------------------------------------------------------------------
# # You may also want density plots to visualize the parameter distributions
# # -------------------------------------------------------------------
# 
# # Function to create a density plot for a specific parameter
# plot_parameter_densities <- function(param_name) {
#   # param_name might be "alpha_pre", "alpha_post", "beta_pre", or "beta_post"
#   param_data <- data.frame(
#     value = c(draws_df[[paste0(param_name, "_G1")]],
#               draws_df[[paste0(param_name, "_G2")]],
#               draws_df[[paste0(param_name, "_G3")]]),
#     Group = factor(rep(c("Group 1 (AN)", "Group 2 (HC)", "Group 3 (RI)"), 
#                        each = nrow(draws_df)))
#   )
#   
#   ggplot(param_data, aes(x = value, fill = Group)) +
#     geom_density(alpha = 0.5) +
#     labs(
#       title = paste("Posterior Distributions for", param_name),
#       x = "Parameter Value",
#       y = "Density"
#     ) +
#     theme_minimal()
# }
# 
# # -------------------------------------------------------------------
# # Create density plots for each parameter
# # -------------------------------------------------------------------
# plot_parameter_densities("alpha_pre")
# plot_parameter_densities("alpha_post")
# plot_parameter_densities("beta_pre")
# plot_parameter_densities("beta_post")

# eof ---


