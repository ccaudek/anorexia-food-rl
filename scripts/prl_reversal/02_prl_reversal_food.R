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
  here::here("scripts", "prl_reversal", "stan", "prl_reversal_2.stan")
) 
mod$print()


# Run sampling ------------------------------------------------------------

# Function to generate reasonable initial values
init_func <- function() {
  list(
    # Group-level parameters in raw space (logit/log)
    baseline_alpha_pos_raw = rep(0, G),  # logit(0.5) = 0
    delta_alpha_pos_raw = rep(0, G),     # logit(0.5) = 0
    baseline_alpha_neg_raw = rep(0, G),  # logit(0.5) = 0
    delta_alpha_neg_raw = rep(0, G),     # logit(0.5) = 0
    baseline_beta_raw = rep(0, G),       # log(1) = 0
    delta_beta_raw = rep(0, G),          # log(1) = 0
    
    # Standard deviations (small positive values)
    sigma_baseline_alpha_pos = rep(0.2, G),
    sigma_delta_alpha_pos = rep(0.2, G),
    sigma_baseline_alpha_neg = rep(0.2, G),
    sigma_delta_alpha_neg = rep(0.2, G),
    sigma_baseline_beta = rep(0.2, G),
    sigma_delta_beta = rep(0.2, G),
    
    # Z-scores (standard normal)
    z_baseline_alpha_pos = rep(0, S_food),  # Adjust S based on your dataset
    z_delta_alpha_pos = rep(0, S_food),
    z_baseline_alpha_neg = rep(0, S_food),
    z_delta_alpha_neg = rep(0, S_food),
    z_baseline_beta = rep(0, S_food),
    z_delta_beta = rep(0, S_food)
  )
}

# Sample using initial values
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


# -------------------------------------------------------------------
# Extract posterior draws for group-level parameters
# -------------------------------------------------------------------
# Suppose your groups are indexed from 1 to G
G <- 3  # example with 2 groups; set this to match your data

# Get draws for the relevant parameters
# This returns an array of MCMC draws
param_names <- c("baseline_alpha_pos", "delta_alpha_pos",
                 "baseline_alpha_neg", "delta_alpha_neg",
                 "baseline_beta",      "delta_beta")
draws_array <- fit_food$draws(variables = param_names)

# Convert to a data frame of draws for easier manipulation
draws_df <- as_draws_df(draws_array)

# -------------------------------------------------------------------
# Create columns for the transformed parameters 
#    (before vs after reversal) for each group
# -------------------------------------------------------------------
# We'll use logistic=plogis(...) for alpha, and exp(...) for beta.
# This loop simply adds new columns to 'draws_df': 
#   alpha_pos_pre_G1, alpha_pos_post_G1, alpha_pos_pre_G2, etc.
for(i in seq_len(G)) {
  # learning rates (alpha_pos)
  draws_df <- draws_df %>%
    mutate(
      !!paste0("alpha_pos_pre_G", i)  := plogis(.data[[paste0("baseline_alpha_pos[", i, "]")]]),
      !!paste0("alpha_pos_post_G", i) := plogis(.data[[paste0("baseline_alpha_pos[", i, "]")]] +
                                                  .data[[paste0("delta_alpha_pos[", i, "]")]])
    )
  # learning rates (alpha_neg)
  draws_df <- draws_df %>%
    mutate(
      !!paste0("alpha_neg_pre_G", i)  := plogis(.data[[paste0("baseline_alpha_neg[", i, "]")]]),
      !!paste0("alpha_neg_post_G", i) := plogis(.data[[paste0("baseline_alpha_neg[", i, "]")]] +
                                                  .data[[paste0("delta_alpha_neg[", i, "]")]])
    )
  # temperature (beta)
  draws_df <- draws_df %>%
    mutate(
      !!paste0("beta_pre_G", i)  := exp(.data[[paste0("baseline_beta[", i, "]")]]),
      !!paste0("beta_post_G", i) := exp(.data[[paste0("baseline_beta[", i, "]")]] +
                                          .data[[paste0("delta_beta[", i, "]")]])
    )
}

# -------------------------------------------------------------------
# Summarize each transformed parameter with posterior means 
#    and 95% credible intervals
# -------------------------------------------------------------------
# A simple way: use 'summarize_draws()' from the posterior package:
param_prefixes <- c("alpha_pos_pre_", "alpha_pos_post_", 
                    "alpha_neg_pre_", "alpha_neg_post_", 
                    "beta_pre_",      "beta_post_")

# Grab only the columns that hold these derived parameters:
derived_param_cols <- purrr::map(param_prefixes, ~grep(.x, names(draws_df), value=TRUE)) %>%
  unlist()

summaries <- draws_df %>%
  dplyr::select(all_of(derived_param_cols)) %>%
  summarize_draws(
    mean,
    ~quantile2(.x, probs = c(0.025, 0.975)) # 95% credible intervals
  )

# 'summaries' is now a data frame with columns:
#   - variable
#   - mean
#   - q2.5   (2.5% quantile)
#   - q97.5  (97.5% quantile)
#
# Example of how to look at it:
print(summaries, n=100)

# ----------------------------------------------------------
# Plot the posterior means and 95% CIs
# ----------------------------------------------------------

# ----------------------------------------------------------
# Prepare data subsets for alpha_pos, alpha_neg, beta
# ----------------------------------------------------------

## a) Positive learning rates (alpha_pos)
alpha_pos_df <- summaries %>%
  dplyr::filter(grepl("alpha_pos", variable)) %>%
  mutate(
    # Distinguish "pre" vs "post"
    phase = ifelse(grepl("pre", variable), "pre", "post"),
    # Extract the group number (the character after "..._G")
    group_num = sub(".*_G", "", variable),
    # Map group numbers to labels
    group = case_when(
      group_num == "1" ~ "AN",
      group_num == "2" ~ "HC",
      group_num == "3" ~ "RI",
      TRUE ~ group_num  # fallback, if more groups
    )
  )

## b) Negative learning rates (alpha_neg)
alpha_neg_df <- summaries %>%
  dplyr::filter(grepl("alpha_neg", variable)) %>%
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

## c) Temperature (beta)
beta_df <- summaries %>%
  dplyr::filter(grepl("beta", variable)) %>%
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

# ----------------------------------------------------------
# Plot function for each parameter family
#    - We do separate plots (no subplots/facets).
# ----------------------------------------------------------

plot_param <- function(df, ylab_title, plot_title) {
  ggplot(df, aes(x = group, y = mean, shape = phase)) +
    # Points for posterior mean
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    # Error bars for 95% CI
    geom_errorbar(
      aes(ymin = q2.5, ymax = q97.5),
      position = position_dodge(width = 0.3),
      width = 0.2
    ) +
    labs(
      x = "Group",
      y = ylab_title,
      title = plot_title
    ) +
    theme_minimal()
}

# ----------------------------------------------------------
# Produce the three separate plots
# ----------------------------------------------------------

# Positive learning rates
p_alpha_pos <- plot_param(
  df = alpha_pos_df,
  ylab_title = expression(alpha[pos]),
  plot_title = "Positive Learning Rate (alpha_pos): Pre vs. Post"
)

# Negative learning rates
p_alpha_neg <- plot_param(
  df = alpha_neg_df,
  ylab_title = expression(alpha[neg]),
  plot_title = "Negative Learning Rate (alpha_neg): Pre vs. Post"
)

# Temperature
p_beta <- plot_param(
  df = beta_df,
  ylab_title = expression(beta),
  plot_title = "Temperature (beta): Pre vs. Post"
)

# ----------------------------------------------------------
# Print/show each plot individually
# ----------------------------------------------------------
p_alpha_pos
p_alpha_neg
p_beta


# -------------------------------------------------------------------
# Compute posterior probabilities that Group 1 (AN) < Group 2 and Group 3
# -------------------------------------------------------------------

# Create a dataframe to store the results
prob_results <- data.frame(
  parameter = character(),
  comparison = character(),
  probability = numeric(),
  stringsAsFactors = FALSE
)

# Define the parameters to compare
parameters <- c("alpha_pos_pre", "alpha_pos_post", 
                "alpha_neg_pre", "alpha_neg_post", 
                "beta_pre", "beta_post")

# For each parameter, compute P(Group1 < Group2) and P(Group1 < Group3)
for (param in parameters) {
  # Compare Group 1 vs Group 2
  g1_param <- draws_df[[paste0(param, "_G1")]]
  g2_param <- draws_df[[paste0(param, "_G2")]]
  prob_g1_lt_g2 <- mean(g1_param < g2_param)
  
  # Compare Group 1 vs Group 3
  g3_param <- draws_df[[paste0(param, "_G3")]]
  prob_g1_lt_g3 <- mean(g1_param < g3_param)
  
  # Add to results dataframe
  prob_results <- rbind(prob_results, 
                        data.frame(parameter = param,
                                   comparison = "G1 < G2",
                                   probability = prob_g1_lt_g2),
                        data.frame(parameter = param,
                                   comparison = "G1 < G3",
                                   probability = prob_g1_lt_g3))
}

# Print the results
print(prob_results, row.names = FALSE)

# You can also format the probabilities as percentages for easier interpretation
prob_results$formatted_prob <- paste0(round(prob_results$probability * 100, 1), "%")
print(prob_results, row.names = FALSE)

# Optional: Calculate Bayes factors for evidence that G1 < G2 or G1 < G3
# Bayes factor can be approximated as p/(1-p) when testing against a point null
prob_results$bayes_factor <- prob_results$probability / (1 - prob_results$probability)
print(prob_results, row.names = FALSE)
# parameter     comparison probability formatted_prob bayes_factor parameter_label
# alpha_pos_pre    G1 < G2     0.98225          98.2%   55.3380282          α+ Pre
# alpha_pos_pre    G1 < G3     0.96275          96.3%   25.8456376          α+ Pre
# alpha_pos_post   G1 < G2     0.98800          98.8%   82.3333333         α+ Post
# alpha_pos_post   G1 < G3     0.94525          94.5%   17.2648402         α+ Post
# alpha_neg_pre    G1 < G2     0.99850          99.9%  665.6666667          α- Pre
# alpha_neg_pre    G1 < G3     1.00000           100%          Inf          α- Pre
# alpha_neg_post   G1 < G2     0.99450          99.4%  180.8181818         α- Post
# alpha_neg_post   G1 < G3     0.99800          99.8%  499.0000000         α- Post
# beta_pre         G1 < G2     0.23525          23.5%    0.3076169           β Pre
# beta_pre         G1 < G3     0.46100          46.1%    0.8552876           β Pre
# beta_post        G1 < G2     0.19000            19%    0.2345679          β Post
# beta_post        G1 < G3     0.17275          17.3%    0.2088244          β Post

# -------------------------------------------------------------------
# Create a visualization of the results
# -------------------------------------------------------------------

# Reshape data for visualization
prob_results$parameter_label <- factor(prob_results$parameter, 
                                       levels = parameters,
                                       labels = c("α+ Pre", "α+ Post", 
                                                  "α- Pre", "α- Post", 
                                                  "β Pre", "β Post"))

# Create a heatmap of probabilities
ggplot(prob_results, aes(x = comparison, y = parameter_label, fill = probability)) +
  geom_tile() +
  geom_text(aes(label = formatted_prob), color = "black") +
  scale_fill_gradient2(low = "white", high = "darkblue", 
                       midpoint = 0.5, limits = c(0, 1),
                       name = "Posterior\nProbability") +
  labs(title = "Posterior Probability that Group 1 (AN) < Other Groups",
       x = "Comparison", y = "Parameter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# -------------------------------------------------------------------
# You may also want density plots to visualize the parameter distributions
# -------------------------------------------------------------------

# Function to create a density plot for a specific parameter
plot_parameter_densities <- function(param_name) {
  param_data <- data.frame(
    value = c(draws_df[[paste0(param_name, "_G1")]],
              draws_df[[paste0(param_name, "_G2")]],
              draws_df[[paste0(param_name, "_G3")]]),
    Group = factor(rep(c("Group 1 (AN)", "Group 2", "Group 3"), 
                       each = nrow(draws_df)))
  )
  
  ggplot(param_data, aes(x = value, fill = Group)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Posterior Distributions for", param_name),
         x = "Parameter Value", y = "Density") +
    theme_minimal()
}

# Example: Create density plots for each parameter
# You can run these individually or save them to files
plot_parameter_densities("alpha_pos_pre")
plot_parameter_densities("alpha_pos_post")
plot_parameter_densities("alpha_neg_pre")
plot_parameter_densities("alpha_neg_post")
plot_parameter_densities("beta_pre")
plot_parameter_densities("beta_post")

# eof ---


