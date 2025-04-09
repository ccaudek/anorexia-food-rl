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


# Compile model -----------------------------------------------------------

mod <- cmdstan_model(
  here::here("scripts", "prl_reversal", "stan", "prl_one_learning_rate.stan")
) 
mod$print()


# Run sampling ------------------------------------------------------------

init_neutral_func <- function() {
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
    z_baseline_alpha = rep(0, S_neutral),
    z_delta_alpha    = rep(0, S_neutral),
    z_baseline_beta  = rep(0, S_neutral),
    z_delta_beta     = rep(0, S_neutral)
  )
}


fit_neutral <- mod$sample(
  data = stan_data_neutral,
  seed = 123,
  init = rep(list(init_neutral_func()), 4),  # One list per chain
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  adapt_delta = 0.9,  # Increase adapt_delta to reduce step size
  max_treedepth = 12  # Increase max_treedepth to allow more exploration
)


# -------------------------------------------------------------------
# Extract posterior draws for group-level parameters
# -------------------------------------------------------------------
# Suppose your groups are indexed from 1 to G
G <- 3  # (AN, HC, RI)

# We want draws for the group-level alpha & beta parameters
param_names <- c(
  "baseline_alpha", 
  "delta_alpha",
  "baseline_beta", 
  "delta_beta",
  "post_alpha", 
  "post_beta"
)
draws_array <- fit_neutral$draws(variables = param_names)

# Convert to data frame
draws_df <- as_draws_df(draws_array)


# -------------------------------------------------------------------
# Create columns for the transformed parameters 
#    (before vs after reversal) for each group
# -------------------------------------------------------------------
# We'll use logistic=plogis(...) for alpha, and exp(...) for beta.
# This loop simply adds new columns to 'draws_df': 
#   alpha_pos_pre_G1, alpha_pos_post_G1, alpha_pos_pre_G2, etc.
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


# -------------------------------------------------------------------
# Summarize each transformed parameter with posterior means 
#    and 95% credible intervals
# -------------------------------------------------------------------
# A simple way: use 'summarize_draws()' from the posterior package:
# Identify the derived parameter columns we want to summarize
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


# ----------------------------------------------------------
# Plot the posterior means and 95% CIs
# ----------------------------------------------------------

# ----------------------------------------------------------
# Prepare data subsets for alpha_pos, alpha_neg, beta
# ----------------------------------------------------------

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


# We define the single-alpha param sets:
parameters <- c("alpha_pre", "alpha_post", "beta_pre", "beta_post")

prob_results <- data.frame(
  parameter = character(),
  comparison = character(),
  probability = numeric(),
  stringsAsFactors = FALSE
)

for (param in parameters) {
  # e.g. draws_df$alpha_pre_G1, draws_df$alpha_pre_G2
  g1 <- draws_df[[paste0(param, "_G1")]]
  g2 <- draws_df[[paste0(param, "_G2")]]
  g3 <- draws_df[[paste0(param, "_G3")]]
  
  prob_g1_lt_g2 <- mean(g1 < g2)
  prob_g1_lt_g3 <- mean(g1 < g3)
  
  prob_results <- rbind(
    prob_results,
    data.frame(parameter = param, comparison = "G1 < G2", probability = prob_g1_lt_g2),
    data.frame(parameter = param, comparison = "G1 < G3", probability = prob_g1_lt_g3)
  )
}

prob_results$formatted_prob <- paste0(round(prob_results$probability * 100, 1), "%")
print(prob_results, row.names = FALSE)


# -------------------------------------------------------------------
# You may also want density plots to visualize the parameter distributions
# -------------------------------------------------------------------

# Function to create a density plot for a specific parameter
plot_parameter_densities <- function(param_name) {
  # param_name might be "alpha_pre", "alpha_post", "beta_pre", or "beta_post"
  param_data <- data.frame(
    value = c(draws_df[[paste0(param_name, "_G1")]],
              draws_df[[paste0(param_name, "_G2")]],
              draws_df[[paste0(param_name, "_G3")]]),
    Group = factor(rep(c("Group 1 (AN)", "Group 2 (HC)", "Group 3 (RI)"), 
                       each = nrow(draws_df)))
  )
  
  ggplot(param_data, aes(x = value, fill = Group)) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste("Posterior Distributions for", param_name),
      x = "Parameter Value",
      y = "Density"
    ) +
    theme_minimal()
}

# -------------------------------------------------------------------
# Create density plots for each parameter
# -------------------------------------------------------------------
plot_parameter_densities("alpha_pre")
plot_parameter_densities("alpha_post")
plot_parameter_densities("beta_pre")
plot_parameter_densities("beta_post")

# eof ---


