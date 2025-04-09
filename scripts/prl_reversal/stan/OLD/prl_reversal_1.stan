data {
  int<lower=1> S;    // number of subjects
  int<lower=1> G;    // number of groups (3)
  int<lower=1> N;    // total number of trials (pre + post)

  // subject IDs for each trial, in [1..S]
  array[N] int<lower=1, upper=S> subj;   

  // group index for each subject, in [1..G]
  array[S] int<lower=1, upper=G> group;  

  // choice and feedback, each in {0,1}
  array[N] int<lower=0, upper=1> choice;   
  array[N] int<lower=0, upper=1> feedback;

  // epoch: 1=pre, 2=post
  array[N] int<lower=1, upper=2> epoch;
}

parameters {
  //------------------------------------------------------------
  // Group-level parameters (raw scale) for alpha and beta
  // We store "raw" means for alpha in logit space, beta in log space

  // alpha+ PRE
  vector[G] mu_alpha_pos_pre_raw;        
  array[G] real<lower=0> sigma_alpha_pos_pre; 

  // alpha+ POST
  vector[G] mu_alpha_pos_post_raw;
  array[G] real<lower=0> sigma_alpha_pos_post;

  // alpha- PRE
  vector[G] mu_alpha_neg_pre_raw;
  array[G] real<lower=0> sigma_alpha_neg_pre;

  // alpha- POST
  vector[G] mu_alpha_neg_post_raw;
  array[G] real<lower=0> sigma_alpha_neg_post;

  // beta PRE
  vector[G] mu_beta_pre_raw;     
  array[G] real<lower=0> sigma_beta_pre;

  // beta POST
  vector[G] mu_beta_post_raw;
  array[G] real<lower=0> sigma_beta_post;

  //------------------------------------------------------------
  // Subject-level latent parameters (non-centered)
  vector[S] z_alpha_pos_pre;  
  vector[S] z_alpha_pos_post; 
  vector[S] z_alpha_neg_pre;  
  vector[S] z_alpha_neg_post;
  vector[S] z_beta_pre;
  vector[S] z_beta_post;
}

transformed parameters {
  //------------------------------------------------------------
  // Transform group-level means:
  // alpha in [0,1] (inv_logit), beta in (0,∞) (exp)

  // alpha+ pre/post
  vector[G] mu_alpha_pos_pre;  
  vector[G] mu_alpha_pos_post;

  // alpha- pre/post
  vector[G] mu_alpha_neg_pre;  
  vector[G] mu_alpha_neg_post;

  // beta pre/post
  vector[G] mu_beta_pre; 
  vector[G] mu_beta_post;

  //------------------------------------------------------------
  // Subject-level parameters in the natural space
  array[S] real<lower=0,upper=1> alpha_pos_pre;
  array[S] real<lower=0,upper=1> alpha_pos_post;
  array[S] real<lower=0,upper=1> alpha_neg_pre;
  array[S] real<lower=0,upper=1> alpha_neg_post;
  array[S] real<lower=0> beta_pre;
  array[S] real<lower=0> beta_post;

  //------------------------------------------------------------
  // 1) Convert the group-level means
  for(g in 1:G) {
    // alpha
    mu_alpha_pos_pre[g]  = inv_logit(mu_alpha_pos_pre_raw[g]);
    mu_alpha_pos_post[g] = inv_logit(mu_alpha_pos_post_raw[g]);
    mu_alpha_neg_pre[g]  = inv_logit(mu_alpha_neg_pre_raw[g]);
    mu_alpha_neg_post[g] = inv_logit(mu_alpha_neg_post_raw[g]);

    // beta
    mu_beta_pre[g]  = exp(mu_beta_pre_raw[g]);
    mu_beta_post[g] = exp(mu_beta_post_raw[g]);
  }

  //------------------------------------------------------------
  // 2) Compute subject-level parameters (non-centered)
  for(s in 1:S) {
    int g_s = group[s]; // subject s's group index
    
    // alpha+ pre
    alpha_pos_pre[s] = inv_logit(
      logit(mu_alpha_pos_pre[g_s]) 
      + sigma_alpha_pos_pre[g_s] * z_alpha_pos_pre[s]
    );
    
    // alpha+ post
    alpha_pos_post[s] = inv_logit(
      logit(mu_alpha_pos_post[g_s])
      + sigma_alpha_pos_post[g_s] * z_alpha_pos_post[s]
    );

    // alpha- pre
    alpha_neg_pre[s] = inv_logit(
      logit(mu_alpha_neg_pre[g_s])
      + sigma_alpha_neg_pre[g_s] * z_alpha_neg_pre[s]
    );

    // alpha- post
    alpha_neg_post[s] = inv_logit(
      logit(mu_alpha_neg_post[g_s])
      + sigma_alpha_neg_post[g_s] * z_alpha_neg_post[s]
    );

    // beta pre
    beta_pre[s] = exp(
      log(mu_beta_pre[g_s]) + sigma_beta_pre[g_s] * z_beta_pre[s]
    );

    // beta post
    beta_post[s] = exp(
      log(mu_beta_post[g_s]) + sigma_beta_post[g_s] * z_beta_post[s]
    );
  }
}

model {
  //------------------------------------------------------------
  // Priors on group-level raw means
  mu_alpha_pos_pre_raw  ~ normal(0, 1);
  mu_alpha_pos_post_raw ~ normal(0, 1);
  mu_alpha_neg_pre_raw  ~ normal(0, 1);
  mu_alpha_neg_post_raw ~ normal(0, 1);

  mu_beta_pre_raw  ~ normal(0, 1);
  mu_beta_post_raw ~ normal(0, 1);

  //------------------------------------------------------------
  // Tighter priors on the group-level sigmas (half-normal)
  // You can adjust the scale if needed (0.5 -> 0.2 if you want even tighter).
  sigma_alpha_pos_pre  ~ normal(0, 0.5);
  sigma_alpha_pos_post ~ normal(0, 0.5);
  sigma_alpha_neg_pre  ~ normal(0, 0.5);
  sigma_alpha_neg_post ~ normal(0, 0.5);
  sigma_beta_pre       ~ normal(0, 0.5);
  sigma_beta_post      ~ normal(0, 0.5);

  //------------------------------------------------------------
  // Priors on subject-level effects (standard normal)
  z_alpha_pos_pre  ~ normal(0,1);
  z_alpha_pos_post ~ normal(0,1);
  z_alpha_neg_pre  ~ normal(0,1);
  z_alpha_neg_post ~ normal(0,1);
  z_beta_pre       ~ normal(0,1);
  z_beta_post      ~ normal(0,1);

  //------------------------------------------------------------
  // Likelihood (forward Q-value update)
  // Starting Q-values are 0.5 each, or you could pass them from data
  for(s in 1:S) {
    real Q_food = 0.5;
    real Q_neutral = 0.5;
    
    for(n in 1:N) {
      if(subj[n] == s) {
        // select the right alpha+/alpha-/beta depending on epoch
        real alpha_pos = (epoch[n] == 1)
                         ? alpha_pos_pre[s]
                         : alpha_pos_post[s];
        real alpha_neg = (epoch[n] == 1)
                         ? alpha_neg_pre[s]
                         : alpha_neg_post[s];
        real beta      = (epoch[n] == 1)
                         ? beta_pre[s]
                         : beta_post[s];

        // Probability of choosing "food" via softmax for 2 options
        real p_food = inv_logit( beta * (Q_food - Q_neutral) );
        
        // choice likelihood
        target += bernoulli_lpmf(choice[n] | p_food);

        // update Q based on the chosen option
        if(choice[n] == 1) {
          // prediction error = feedback - Q_food
          real pe = feedback[n] - Q_food;
          if(pe > 0)
            Q_food += alpha_pos * pe;
          else
            Q_food += alpha_neg * pe;
        } else {
          // choice[n] == 0 => neutral chosen
          real pe = feedback[n] - Q_neutral;
          if(pe > 0)
            Q_neutral += alpha_pos * pe;
          else
            Q_neutral += alpha_neg * pe;
        }
      }
    }
  }
}

generated quantities {
  // log-likelihood for each trial (useful for LOO, WAIC, etc.)
  array[N] real log_lik;
  
  {
    for(n in 1:N)
      log_lik[n] = 0; 
    
    for(s in 1:S) {
      real Q_food = 0.5;
      real Q_neutral = 0.5;
      
      for(n in 1:N) {
        if(subj[n] == s) {
          real alpha_pos = (epoch[n] == 1) ? alpha_pos_pre[s] : alpha_pos_post[s];
          real alpha_neg = (epoch[n] == 1) ? alpha_neg_pre[s] : alpha_neg_post[s];
          real beta      = (epoch[n] == 1) ? beta_pre[s] : beta_post[s];
          
          real p_food = inv_logit( beta * (Q_food - Q_neutral) );
          
          // store log-lik
          log_lik[n] = bernoulli_lpmf(choice[n] | p_food);
          
          // update Q
          if(choice[n] == 1) {
            real pe = feedback[n] - Q_food;
            if(pe > 0)
              Q_food += alpha_pos * pe;
            else
              Q_food += alpha_neg * pe;
          } else {
            real pe = feedback[n] - Q_neutral;
            if(pe > 0)
              Q_neutral += alpha_pos * pe;
            else
              Q_neutral += alpha_neg * pe;
          }
        }
      }
    }
  }
}
