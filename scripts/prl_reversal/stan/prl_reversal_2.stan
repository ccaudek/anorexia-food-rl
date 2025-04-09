data {
  int<lower=1> S; // numero soggetti
  int<lower=1> G; // numero gruppi (3)
  int<lower=1> N; // numero totale di trial
  array[N] int<lower=1, upper=S> subj; // soggetto di ciascun trial
  array[S] int<lower=1, upper=G> group; // gruppo (1..G) di ciascun soggetto
  array[N] int<lower=0, upper=1> choice; // scelta 0/1
  array[N] int<lower=0, upper=1> feedback; // esito 0/1
  array[N] int<lower=1, upper=2> epoch; // 1=pre, 2=post
}

parameters {
  //------------------------------------------------------------
  // PARAMETRI DI GRUPPO (livello "baseline" e "delta") in logit/log space
  // Vogliamo per ognuna delle 3 variabili (alpha+, alpha-, beta):
  // baseline_X[g], delta_X[g] in R (poi li trasformeremo)
  
  // alpha+ (logit space)
  vector[G] baseline_alpha_pos_raw;
  vector[G] delta_alpha_pos_raw;
  
  // alpha- (logit space)
  vector[G] baseline_alpha_neg_raw;
  vector[G] delta_alpha_neg_raw;
  
  // beta (log space)
  vector[G] baseline_beta_raw;
  vector[G] delta_beta_raw;
  
  //------------------------------------------------------------
  // Deviazioni standard (non-cent. approach) per baseline e delta
  // Potremmo dare priors half-normal più stretti per stabilizzare
  array[G] real<lower=0> sigma_baseline_alpha_pos;
  array[G] real<lower=0> sigma_delta_alpha_pos;
  array[G] real<lower=0> sigma_baseline_alpha_neg;
  array[G] real<lower=0> sigma_delta_alpha_neg;
  array[G] real<lower=0> sigma_baseline_beta;
  array[G] real<lower=0> sigma_delta_beta;
  
  //------------------------------------------------------------
  // EFFETTI SOGGETTO-LIVELLO (non centrati)
  // Per ognuna delle 6 "componenti" (baseline e delta di alpha+ / alpha- / beta)
  vector[S] z_baseline_alpha_pos;
  vector[S] z_delta_alpha_pos;
  vector[S] z_baseline_alpha_neg;
  vector[S] z_delta_alpha_neg;
  vector[S] z_baseline_beta;
  vector[S] z_delta_beta;
}

transformed parameters {
  //------------------------------------------------------------
  // TRASFORMAZIONI DI GRUPPO
  // baseline_alpha_pos_group[g] = logit^-1 ( baseline_alpha_pos_raw[g] )
  // delta_alpha_pos_group[g] = logit^-1 ( delta_alpha_pos_raw[g] ), etc.
  vector[G] baseline_alpha_pos_group;
  vector[G] delta_alpha_pos_group;
  vector[G] baseline_alpha_neg_group;
  vector[G] delta_alpha_neg_group;
  vector[G] baseline_beta_group;
  vector[G] delta_beta_group;
  
  for(g in 1:G) {
    baseline_alpha_pos_group[g] = baseline_alpha_pos_raw[g];
    // "raw" in R, => interpretazione logit(baseline).
    // Se preferisci, potresti già prendere logistic(baseline_alpha_pos_raw[g])
    // in un'altra variabile, ma qui rimaniamo in logit-space a livello di group.
    delta_alpha_pos_group[g] = delta_alpha_pos_raw[g];
    // idem, logit-space
    baseline_alpha_neg_group[g] = baseline_alpha_neg_raw[g];
    delta_alpha_neg_group[g] = delta_alpha_neg_raw[g];
    baseline_beta_group[g] = baseline_beta_raw[g];
    delta_beta_group[g] = delta_beta_raw[g];
  }
  
  //------------------------------------------------------------
  // PARAMETRI A LIVELLO SOGGETTO
  // Usando non-centered parametrization per migliorare la convergenza
  // Calcolo parametri individuali di ogni soggetto in logit/log space
  vector[S] baseline_alpha_pos_subj_raw;  // logit-space
  vector[S] delta_alpha_pos_subj_raw;     // logit-space
  vector[S] baseline_alpha_neg_subj_raw;  // logit-space
  vector[S] delta_alpha_neg_subj_raw;     // logit-space
  vector[S] baseline_beta_subj_raw;       // log-space
  vector[S] delta_beta_subj_raw;          // log-space
  
  // Parametri individuali in scale originale (inv_logit/exp)
  vector<lower=0, upper=1>[S] baseline_alpha_pos_subj;
  vector<lower=0, upper=1>[S] delta_alpha_pos_subj;
  vector<lower=0, upper=1>[S] baseline_alpha_neg_subj;
  vector<lower=0, upper=1>[S] delta_alpha_neg_subj;
  vector<lower=0>[S] baseline_beta_subj;
  vector<lower=0>[S] delta_beta_subj;
  
  // Parametri finali usati nel modello per ogni epoca
  vector<lower=0, upper=1>[S] alpha_pos_pre;  // tasso apprendimento rinforzo positivo pre
  vector<lower=0, upper=1>[S] alpha_pos_post; // tasso apprendimento rinforzo positivo post
  vector<lower=0, upper=1>[S] alpha_neg_pre;  // tasso apprendimento rinforzo negativo pre
  vector<lower=0, upper=1>[S] alpha_neg_post; // tasso apprendimento rinforzo negativo post
  vector<lower=0>[S] beta_pre;               // sensibilità rinforzo pre
  vector<lower=0>[S] beta_post;              // sensibilità rinforzo post
  
  // Calcolo parametri soggetto in raw-scale (non-centered approach)
  for (s in 1:S) {
    int g = group[s];
    
    // Baseline + z-score*sigma (non-centered parametrization)
    baseline_alpha_pos_subj_raw[s] = baseline_alpha_pos_group[g] + 
                                    z_baseline_alpha_pos[s] * sigma_baseline_alpha_pos[g];
    delta_alpha_pos_subj_raw[s] = delta_alpha_pos_group[g] + 
                                 z_delta_alpha_pos[s] * sigma_delta_alpha_pos[g];
    
    baseline_alpha_neg_subj_raw[s] = baseline_alpha_neg_group[g] + 
                                    z_baseline_alpha_neg[s] * sigma_baseline_alpha_neg[g];
    delta_alpha_neg_subj_raw[s] = delta_alpha_neg_group[g] + 
                                 z_delta_alpha_neg[s] * sigma_delta_alpha_neg[g];
    
    baseline_beta_subj_raw[s] = baseline_beta_group[g] + 
                               z_baseline_beta[s] * sigma_baseline_beta[g];
    delta_beta_subj_raw[s] = delta_beta_group[g] + 
                            z_delta_beta[s] * sigma_delta_beta[g];
    
    // Trasformazione a scale originale
    baseline_alpha_pos_subj[s] = inv_logit(baseline_alpha_pos_subj_raw[s]);
    delta_alpha_pos_subj[s] = inv_logit(delta_alpha_pos_subj_raw[s]);
    baseline_alpha_neg_subj[s] = inv_logit(baseline_alpha_neg_subj_raw[s]);
    delta_alpha_neg_subj[s] = inv_logit(delta_alpha_neg_subj_raw[s]);
    baseline_beta_subj[s] = exp(baseline_beta_subj_raw[s]);
    delta_beta_subj[s] = exp(delta_beta_subj_raw[s]);
    
    // Parametri finali per le due epoche
    alpha_pos_pre[s] = baseline_alpha_pos_subj[s];
    alpha_pos_post[s] = baseline_alpha_pos_subj[s] * delta_alpha_pos_subj[s];
    
    alpha_neg_pre[s] = baseline_alpha_neg_subj[s];
    alpha_neg_post[s] = baseline_alpha_neg_subj[s] * delta_alpha_neg_subj[s];
    
    beta_pre[s] = baseline_beta_subj[s];
    beta_post[s] = baseline_beta_subj[s] * delta_beta_subj[s];
  }
}

model {
  // Priors per i parametri di gruppo
  baseline_alpha_pos_raw ~ normal(0, 1);  // prior per logit(baseline_alpha_pos)
  delta_alpha_pos_raw ~ normal(0, 1);     // prior per logit(delta_alpha_pos)
  baseline_alpha_neg_raw ~ normal(0, 1);  // prior per logit(baseline_alpha_neg)
  delta_alpha_neg_raw ~ normal(0, 1);     // prior per logit(delta_alpha_neg)
  baseline_beta_raw ~ normal(0, 1);       // prior per log(baseline_beta)
  delta_beta_raw ~ normal(0, 1);          // prior per log(delta_beta)
  
  // Priors per le deviazioni standard
  for (g in 1:G) {
    sigma_baseline_alpha_pos[g] ~ normal(0, 0.5)T[0,];  // half-normal
    sigma_delta_alpha_pos[g] ~ normal(0, 0.5)T[0,];     // half-normal
    sigma_baseline_alpha_neg[g] ~ normal(0, 0.5)T[0,];  // half-normal
    sigma_delta_alpha_neg[g] ~ normal(0, 0.5)T[0,];     // half-normal
    sigma_baseline_beta[g] ~ normal(0, 0.5)T[0,];       // half-normal
    sigma_delta_beta[g] ~ normal(0, 0.5)T[0,];          // half-normal
  }
  
  // Priors per z-scores (std normal)
  z_baseline_alpha_pos ~ normal(0, 1);
  z_delta_alpha_pos ~ normal(0, 1);
  z_baseline_alpha_neg ~ normal(0, 1);
  z_delta_alpha_neg ~ normal(0, 1);
  z_baseline_beta ~ normal(0, 1);
  z_delta_beta ~ normal(0, 1);
  
  // Likelihood - Reinforcement Learning con aggiornamento
  {
    array[S] vector[2] Q;          // Q-values per ogni soggetto e opzione
    real pe;                       // prediction error
    real alpha;                    // tasso apprendimento (alpha+ o alpha-)
    
    // Inizializza Q-values a 0.5 per tutti i soggetti
    for (s in 1:S) {
      Q[s] = rep_vector(0.5, 2);
    }
    
    // Loop per ogni trial
    for (n in 1:N) {
      int s = subj[n];       // soggetto corrente
      int c = choice[n] + 1; // opzione scelta (1 o 2)
      int nc = 3 - c;        // opzione non scelta (2 o 1)
      real beta;             // sensibilità rinforzo
      
      // Seleziona parametri in base all'epoca
      if (epoch[n] == 1) {
        beta = beta_pre[s];
        alpha = feedback[n] == 1 ? alpha_pos_pre[s] : alpha_neg_pre[s];
      } else {
        beta = beta_post[s];
        alpha = feedback[n] == 1 ? alpha_pos_post[s] : alpha_neg_post[s];
      }
      
      // Likelihood della scelta
      choice[n] ~ bernoulli_logit(beta * (Q[s][2] - Q[s][1]));
      
      // Aggiornamento Q-values
      pe = feedback[n] - Q[s][c];
      Q[s][c] += alpha * pe;
    }
  }
}

generated quantities {
  // Trasformazione dei parametri group-level in scala originale
  vector<lower=0, upper=1>[G] baseline_alpha_pos = inv_logit(baseline_alpha_pos_raw);
  vector<lower=0, upper=1>[G] delta_alpha_pos = inv_logit(delta_alpha_pos_raw);
  vector<lower=0, upper=1>[G] baseline_alpha_neg = inv_logit(baseline_alpha_neg_raw);
  vector<lower=0, upper=1>[G] delta_alpha_neg = inv_logit(delta_alpha_neg_raw);
  vector<lower=0>[G] baseline_beta = exp(baseline_beta_raw);
  vector<lower=0>[G] delta_beta = exp(delta_beta_raw);
  
  // Post-epoch parametri group-level
  vector<lower=0, upper=1>[G] post_alpha_pos;
  vector<lower=0, upper=1>[G] post_alpha_neg;
  vector<lower=0>[G] post_beta;
  
  // Log-likelihood per valutazione modello
  vector[N] log_lik;
  
  // Predizioni per posterior predictive checks
  array[N] int y_pred;
  
  // Calcolo parametri post-epoch gruppo
  for (g in 1:G) {
    post_alpha_pos[g] = baseline_alpha_pos[g] * delta_alpha_pos[g];
    post_alpha_neg[g] = baseline_alpha_neg[g] * delta_alpha_neg[g];
    post_beta[g] = baseline_beta[g] * delta_beta[g];
  }
  
  // Calcolo log-likelihood e predizioni
  {
    array[S] vector[2] Q;  // Q-values per ogni soggetto e opzione
    real pe;               // prediction error
    real alpha;            // tasso apprendimento (alpha+ o alpha-)
    
    // Inizializza Q-values a 0.5 per tutti i soggetti
    for (s in 1:S) {
      Q[s] = rep_vector(0.5, 2);
    }
    
    // Loop per ogni trial
    for (n in 1:N) {
      int s = subj[n];       // soggetto corrente
      int c = choice[n] + 1; // opzione scelta (1 o 2)
      int nc = 3 - c;        // opzione non scelta (2 o 1)
      real beta;             // sensibilità rinforzo
      real p;                // probabilità di scegliere opzione 1
      
      // Seleziona parametri in base all'epoca
      if (epoch[n] == 1) {
        beta = beta_pre[s];
        alpha = feedback[n] == 1 ? alpha_pos_pre[s] : alpha_neg_pre[s];
      } else {
        beta = beta_post[s];
        alpha = feedback[n] == 1 ? alpha_pos_post[s] : alpha_neg_post[s];
      }
      
      // Calcolo probabilità
      p = inv_logit(beta * (Q[s][2] - Q[s][1]));
      
      // Log-likelihood
      log_lik[n] = bernoulli_logit_lpmf(choice[n] | beta * (Q[s][2] - Q[s][1]));
      
      // Predizione
      y_pred[n] = bernoulli_rng(p);
      
      // Aggiornamento Q-values
      pe = feedback[n] - Q[s][c];
      Q[s][c] += alpha * pe;
    }
  }
}
