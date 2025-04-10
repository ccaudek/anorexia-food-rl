# Qualitative trends of the data and model's fit.

library(tidyverse)
library(rio)
library(here)
library(missForest)


d1 <- rio::import(here::here("data", "raw", "prl_tot_raw_data.rds"))

participants_names <- rio::import(
  here("data", "raw", "participants_names.RDS")
)

# Select participants used in the paper
d <- d1[d1$subj_name %in% participants_names, ]
length(unique(d$subj_name))

d$is_positive_feedback |> table()

# Recode
d <- d %>%
  mutate(
    is_positive_feedback_numeric = 
      dplyr::recode(is_positive_feedback,
      "no" = 0,
      "yes" = 1,
      "no response" = NA_real_
  )
)

d$subj_name <- factor(d$subj_name)
d$code_psytoolkit <- factor(d$code_psytoolkit)
d$stimulus <- factor(d$stimulus)
d$V1 <- factor(d$V1)
d$V2 <- factor(d$V2)
d$V3 <- factor(d$V3)
d$V4 <- factor(d$V4)
d$V5 <- factor(d$V5)
d$V6 <- factor(d$V6)
d$target_position <- factor(d$target_position)
d$stimulus_type <- factor(d$stimulus_type)
d$stimulus_class <- factor(d$stimulus_class)
d$which_image_is_rewarded_more <- factor(d$which_image_is_rewarded_more)
d$image_category <- factor(d$image_category)
d$is_target_img_rewarded_in_first_epoch <- as.numeric(d$is_target_img_rewarded_in_first_epoch)
d$position_target_img <- factor(d$position_target_img)
d$resp <- factor(d$resp)
d$is_positive_feedback <- factor(d$is_positive_feedback)
d$group <- factor(d$group)
d$diag_cat <- factor(d$diag_cat)

sapply(d, function(x) if(is.factor(x)) nlevels(x))

dd <- d
dd$subj_name <- NULL
dd$code_psytoolkit <- NULL

dat <- missForest(dd)

# Calcolo della media di is_positive_feedback_numeric per ciascun trial, solo per HC
media_feedback_per_trial_HC <- d %>%
  dplyr::filter(group == "controls") %>%
  group_by(trial) %>%
  summarise(mean_feedback = mean(is_positive_feedback_numeric, na.rm = TRUE))

plot(
  media_feedback_per_trial_HC$trial,
  media_feedback_per_trial_HC$mean_feedback,
  type = "l",
  ylim = c(0.38, 0.8)
)

media_feedback_per_trial_AN <- d %>%
  dplyr::filter(group == "patients") %>%
  group_by(trial) %>%
  summarise(mean_feedback = mean(is_positive_feedback_numeric, na.rm = TRUE))

plot(
  media_feedback_per_trial_AN$trial,
  media_feedback_per_trial_AN$mean_feedback,
  type = "l",
  ylim = c(0.38, 0.8)
)


simulate_prl <- function(n_subjects = 117, n_trials = 160, alpha = 0.7, beta = 5) {
  n_options <- 2
  rewards <- numeric(n_trials)
  feedback_matrix <- matrix(NA, nrow = n_subjects, ncol = n_trials)
  
  # Define reward contingencies
  reward_probs <- matrix(NA, nrow = n_trials, ncol = 2)
  for (t in 1:n_trials) {
    # reversals at 41, 81, 121
    epoch <- ((t - 1) %/% 40) %% 2
    if (epoch == 0) {
      reward_probs[t, ] <- c(0.8, 0.2)
    } else {
      reward_probs[t, ] <- c(0.2, 0.8)
    }
  }
  
  # Simulate each subject
  for (s in 1:n_subjects) {
    Q <- rep(0.5, n_options)  # initialize expected values
    for (t in 1:n_trials) {
      # Softmax to compute choice probabilities
      probs <- exp(beta * Q) / sum(exp(beta * Q))
      choice <- sample(1:n_options, 1, prob = probs)
      
      # Simulate feedback
      reward <- rbinom(1, 1, prob = reward_probs[t, choice])
      
      # Update RW
      Q[choice] <- Q[choice] + alpha * (reward - Q[choice])
      
      # Store reward
      feedback_matrix[s, t] <- reward
    }
  }
  
  # Compute average feedback per trial
  avg_feedback_per_trial <- colMeans(feedback_matrix)
  
  return(avg_feedback_per_trial)
}

# Simula
set.seed(123)
avg_feedback <- simulate_prl(alpha = 0.8, beta = 5)

# Plot
plot(avg_feedback, type = "l", lwd = 2, col = "blue",
     xlab = "Trial", ylab = "Proportion of positive feedback",
     main = "Simulated proportion of positive feedback across trials")
abline(v = c(41, 81, 121), lty = 2, col = "red")


