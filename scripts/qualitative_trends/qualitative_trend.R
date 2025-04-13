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


# Codici soggetti per ciascuna categoria diagnostica

AN <- c(
  "ca_po_2002_05_25_700_f", "em_or_2003_01_02_101_f", "au_ru_1998_09_21_806_f",
  "ch_na_2007_06_23_908_f", "ch_ma_2001_10_27_332_f", "bi_di_2006_04_20_725_f",
  "as_ga_2005_06_15_329_f", "da_de_1998_08_15_141_m", "cl_pu_2007_05_24_423_f",
  "gr_bo_1996_07_31_547_f", "fr_la_2004_05_17_363_f", "ar_ce_2005_04_20_937_f",
  "fe_sa_2002_05_09_008_f", "am_gu_1999_02_11_937_f", "be_ma_1999_06_15_475_f",
  "fe_ma_1998_06_29_257_f", "ch_ri_1993_05_05_564_f", "gi_za_1992_09_07_575_f",
  "ir_ve_2004_02_09_500_f", "an_de_1998_11_10_289_f", "ma_va_1998_07_04_538_f",
  "ir_pi_2002_01_22_765_f", "em_gr_2002_08_25_628_f", "gi_ma_1999_09_26_585_f",
  "so_be_2008_12_15_399_f", "ir_to_2007_08_01_838_f", "ch_ca_2000_09_26_406_f",
  "em_al_1989_07_27_200_f", "ch_pi_2004_02_25_126_f", "es_bo_2004_07_23_474_f",
  "em_bi_2007_12_28_766_f", "cr_gi_1994_10_14_378_f", "ch_br_1993_10_04_623_f",
  "he_ha_2006_04_21_874_f", "ch_pi_2001_10_08_418_f", "cr_pa_1969_04_12_179_f"
)

HC <- c(
  "co_pr_2002_03_03_902_f", "il_in_1997_10_07_583_f", "fr_se_2001_04_06_835_f",
  "el_ma_2001_07_17_978_f", "vi_te_2001_06_08_644_f", "ni_os_2001_12_10_411_f",
  "an_to_2000_11_30_575_f", "al_zu_2001_03_12_239_f", "ma_in_1996_10_18_776_f",
  "so_go_1997_05_16_135_f", "gi_sa_2001_05_25_391_f", "al_an_1996_06_03_205_f",
  "ra_ma_2000_03_10_886_f", "fe_te_2000_05_17_086_f", "el_li_2001_03_18_661_f",
  "re_ve_2001_03_28_201_f", "bi_ra_2000_09_28_341_f", "vi_mi_2001_11_09_918_f",
  "ch_ca_1998_01_31_179_f", "so_mo_2001_10_27_943_f", "el_ne_2001_09_06_773_f",
  "ir_ma_2001_08_24_646_f", "gi_fi_1996_03_09_339_f", "ma_sc_1998_07_04_531_f",
  "il_ma_2001_07_30_601_f", "al_su_2001_03_07_759_f", "ch_ma_2001_09_16_817_f",
  "ca_ma_2000_05_05_905_f", "fr_pl_2002_02_14_755_f", "be_ba_1995_04_27_656_f",
  "la_sa_2001_11_15_307_f", "al_lo_2001_02_10_286_f", "ni_st_1997_01_23_081_f",
  "al_pa_2000_12_20_624_f", "el_pa_2000_09_28_331_f", "ga_gi_1992_02_10_570_f",
  "ro_pi_2000_09_30_791_f", "ca_va_2001_08_28_797_f", "an_ol_2000_03_06_615_f",
  "la_or_2001_09_18_400_f", "ar_in_1992_07_31_124_f", "sa_pa_2001_05_14_311_f",
  "ir_da_1999_01_23_879_f", "fr_tr_1997_09_19_223_f", "ma_pr_2000_09_18_430_f"
)

RI <- c(
  "gi_ba_2008_01_31_376_f", "ca_fa_1996_03_26_092_f", "fr_au_1987_12_16_221_f",
  "fe_al_1988_05_06_180_f", "lu_mu_1997_03_18_059_f", "fr_bo_1993_09_09_170_f",
  "el_ma_1986_06_14_839_f", "al_zu_1997_04_02_880_f", "de_sc_1992_07_02_116_f",
  "ma_be_1997_09_01_726_f", "fr_ro_1982_08_15_048_f", "gi_po_1998_11_07_576_f",
  "ma_pa_2001_06_11_636_f", "vi_mi_2000_08_21_472_f", "se_pi_2001_01_22_920_f",
  "gi_me_2001_03_31_627_f", "sa_la_1994_11_13_963_f", "ch_lo_2000_09_25_565_f",
  "el_li_1999_09_08_687_f", "ha_ri_2001_07_07_704_f", "ma_pi_2001_05_11_566_f",
  "ma_la_2001_09_12_609_f", "ot_na_1999_03_18_271_f", "ma_ma_2001_07_10_611_f",
  "ed_sc_2001_09_26_034_m", "ca_mi_2001_06_16_988_f", "li_li_2001_12_04_406_f",
  "an_re_2001_08_28_633_f", "sa_sa_2000_11_24_418_m", "gi_sp_1995_10_16_533_f",
  "gi_se_1999_07_09_402_f", "gi_gi_1990_03_28_384_f", "gi_ga_2001_07_20_277_f",
  "ch_ma_1995_08_28_639_f", "ma_ta_2001_05_23_401_f", "gi_va_1992_04_14_174_f"
)

# Creazione del dataframe
id_df <- data.frame(
  subject_code = c(AN, HC, RI),
  diagnostic_category = c(rep("AN", length(AN)), rep("HC", length(HC)), rep("RI", length(RI)))
)

# Unione dei dataframe
d <- d %>%
  left_join(id_df, by = c("subj_name" = "subject_code"))


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

# d$subj_name <- factor(d$subj_name)
# d$code_psytoolkit <- factor(d$code_psytoolkit)
# d$stimulus <- factor(d$stimulus)
# d$V1 <- factor(d$V1)
# d$V2 <- factor(d$V2)
# d$V3 <- factor(d$V3)
# d$V4 <- factor(d$V4)
# d$V5 <- factor(d$V5)
# d$V6 <- factor(d$V6)
# d$target_position <- factor(d$target_position)
# d$stimulus_type <- factor(d$stimulus_type)
# d$stimulus_class <- factor(d$stimulus_class)
# d$which_image_is_rewarded_more <- factor(d$which_image_is_rewarded_more)
# d$image_category <- factor(d$image_category)
# d$is_target_img_rewarded_in_first_epoch <- as.numeric(d$is_target_img_rewarded_in_first_epoch)
# d$position_target_img <- factor(d$position_target_img)
# d$resp <- factor(d$resp)
# d$is_positive_feedback <- factor(d$is_positive_feedback)
# d$group <- factor(d$group)
# d$diag_cat <- factor(d$diag_cat)

d <- d %>%
  mutate(
    stimulus = 
      ifelse(
        stimulus_type == "socialshame", "neutral", 
        ifelse(stimulus_type == "food", "food", NA)
      )
  )
d$stimulus <- factor(d$stimulus)

# Create the new variable is_optimal_choice
d <- d %>%
  mutate(
    # Determine position of the most rewarded image
    optimal_position = case_when(
      which_image_is_rewarded_more == "image1_rewarded7" & target_position == "image1_sx" ~ "sx",
      which_image_is_rewarded_more == "image1_rewarded7" & target_position == "image1_dx" ~ "dx",
      which_image_is_rewarded_more == "image2_rewarded7" & target_position == "image2_sx" ~ "sx",
      which_image_is_rewarded_more == "image2_rewarded7" & target_position == "image2_dx" ~ "dx",
      TRUE ~ NA_character_
    ),
    
    # Compare choice with optimal position
    is_optimal_choice = ifelse(resp == optimal_position, 1, 0)
  )

d %>%
  group_by(diagnostic_category, stimulus) %>%
  summarise(prop_feedback = mean(is_positive_feedback_numeric, na.rm = TRUE))


# Step 1: Calcolo proporzione di scelte ottimali per soggetto
subject_df <- d %>%
  group_by(subj_name, diagnostic_category, stimulus) %>%
  summarise(
    prop_optimal = mean(is_optimal_choice, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Escludo soggetti specifici
subjects_df <- subject_df %>%
  filter(!subj_name %in% c(
    "ma_pi_2001_05_11_566_f", 
    "ed_sc_2001_09_26_034_m", 
    "sa_sa_2000_11_24_418_m"
  ))

# Step 3: Calcolo media e errore standard tra soggetti
summary_df <- subjects_df %>%
  group_by(diagnostic_category, stimulus) %>%
  summarise(
    mean_prop = mean(prop_optimal, na.rm = TRUE),
    se = sd(prop_optimal, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

summary_df

# Step 4: Grafico
ggplot(summary_df, aes(x = diagnostic_category, y = mean_prop, fill = stimulus)) +
  geom_col(position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = mean_prop - se, ymax = mean_prop + se),
                width = 0.2,
                position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(0.45, 0.62)) +
  labs(x = "Categoria Diagnostica", 
       y = "Proporzione media di scelte ottimali", 
       fill = "Stimolo") +
  theme_minimal(base_size = 14)










# ------------------------------------------------------------------------------


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






sapply(d, function(x) if(is.factor(x)) nlevels(x))

dd <- d
dd$subj_name <- NULL
dd$code_psytoolkit <- NULL

dat <- missForest(dd)
