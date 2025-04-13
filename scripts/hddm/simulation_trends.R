library(tidyverse)
library(here)
library(rio)

# I dati empirici sono: 
# diagnostic_category stimulus mean_prop     se
# 1 AN                  food         0.501 0.0275
# 2 AN                  neutral      0.559 0.0207
# 3 HC                  food         0.552 0.0199
# 4 HC                  neutral      0.567 0.0131
# 5 RI                  food         0.562 0.0196
# 6 RI                  neutral      0.593 0.0162 

params <- rio::import(
  here("scripts", "hddm", "params_for_R.csv")
  )
head(params)

params <- params %>%
    mutate(
         pos_alpha = plogis(pos_alpha),
         neg_alpha = plogis(neg_alpha),
         a = exp(a)
         # v_transf = pmin(pmax(v, 0.5), 3)  # forziamo v tra 0.5 e 3
       )

NTRIALS <- 160

simulate_subject <- function(alpha_pos, alpha_neg, v, n_trials = NTRIALS, 
                             p_reward = c(0.8, 0.2), epsilon = 0.0) {
       Q <- c(0, 0)
       choices <- numeric(n_trials)
       rewards <- numeric(n_trials)
       
         for (t in 1:n_trials) {
             p_choice_softmax <- exp(v * Q) / sum(exp(v * Q))
             
               # blend with random choice
               p_choice <- (1 - epsilon) * p_choice_softmax + epsilon * 0.5
               
                 choice <- sample(1:2, size = 1, prob = p_choice)
                 reward <- rbinom(1, 1, p = p_reward[choice])
                 
                   pe <- reward - Q[choice]
                   alpha <- ifelse(pe > 0, alpha_pos, alpha_neg)
                   Q[choice] <- Q[choice] + alpha * pe
                   
                     choices[t] <- choice
                     rewards[t] <- reward
                   }
       
         data.frame(trial = 1:n_trials, choice = choices, reward = rewards)
    }

NSIMS <- 20   # numero di simulazioni per soggetto

# Funzione che simula un soggetto N volte e restituisce la media
simulate_multiple <- function(alpha_pos, alpha_neg, v, n_trials = NTRIALS, nsims = NSIMS) {
  res <- replicate(nsims, {
    dat <- simulate_subject(alpha_pos, alpha_neg, v, n_trials)
    mean(dat$reward)
  })
  mean(res)  # media delle simulazioni
}

set.seed(1234)

sim_results <- params %>%
  group_by(group, condition, subject) %>%
  group_modify(~ {
    prop_correct_mean <- simulate_multiple(
      alpha_pos = .x$pos_alpha,
      alpha_neg = ifelse(is.na(.x$neg_alpha), .x$pos_alpha, .x$neg_alpha),
      v = .x$v
    )
    tibble(prop_correct = prop_correct_mean)
  })


# Barplot con errorbar e limiti y fissati
ggplot(sim_results, aes(x = group, y = prop_correct, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", 
                                position = position_dodge(width = 0.8), 
                                 color = "black") +
     stat_summary(fun.data = mean_se, geom = "errorbar",
                                  position = position_dodge(width = 0.8), 
                                  width = 0.2) +
     coord_cartesian(ylim = c(0.5, 0.65)) +  # Limiti asse y
     theme_minimal(base_size = 14) +
     labs(x = "Gruppo", 
                   y = "Proporzione risposte corrette (media simulata)",
                   fill = "Condizione") +
     scale_fill_brewer(palette = "Set2")


sim_results %>%
  group_by(group, condition) %>%
  summarise(
    mean_prop = mean(prop_correct, na.rm = TRUE),
    se = sd(prop_correct, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
> # group condition mean_prop      se
  > # <chr> <chr>         <dbl>   <dbl>
  > # 1 AN    food          0.558 0.0157 
  > # 2 AN    neutral       0.594 0.0124 
  > # 3 HC    food          0.599 0.00981
  > # 4 HC    neutral       0.605 0.00988
  > # 5 RI    food          0.619 0.0105 
  > # 6 RI    neutral       0.604 0.00937

