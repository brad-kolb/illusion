// varying (random) effect meta-analysis
// adapted from www.mc-stan.org/docs/stan-users-guide/measurement-error.html
data {
  int<lower=0> J; // num studies
  array[J] int<lower=0> n_t; // num cases, treatment
  array[J] int<lower=0> r_t; // num events, treatment
  array[J] int<lower=0> n_c; // num cases, control
  array[J] int<lower=0> r_c; // num events, control
  
  int<lower=0> estimate_posterior; // switch for estimating posterior vs running prior predictive simulation
  int<lower=0> priors; // switch for checking sensitivity of posterior to alternative specification for priors
}
transformed data {
  array[J] real y; // log odds ratio for each study
  for (j in 1:J) {
    y[j] = log(r_t[j]) - log(n_t[j] - r_t[j]) 
    - (log(r_c[j]) - log(n_c[j] - r_c[j]));
  }
  
  array[J] real<lower=0> se; // standard error of y (inverse variance method)
  for (j in 1:J) {
    se[j] = sqrt(1.0 / r_t[j] + 1.0 / (n_t[j] - r_t[j]) 
    + 1.0 / r_c[j] + 1.0 / (n_c[j] - r_c[j]));
  }
}
parameters {
  real mu; // mean treatment effect
  real<lower=0> tau; // deviation of treatment effects from the mean
  vector<offset=mu,multiplier=tau>[J] theta; // trial-specific treatment effects
}
model {
  if (estimate_posterior == 1) {
  y[1:J] ~ normal(theta[1:J], se[1:J]);
  } 
  
  theta[1:J] ~ normal(mu, tau); 
  if (priors == 1) { // standard normal
    mu ~ std_normal(); 
    tau ~ std_normal(); 
  } else { // CDSR
    mu ~ student_t(3.8, 0, 0.48); 
    tau ~ lognormal(-1.44, 0.79); 
  }
}
generated quantities {
  // pooling metrics
  vector[J] se2 = square(to_vector(se)); // approximate sampling variance for each study
  real se2_hat = sum(se2) / J; // average approximate sampling variance across all studies
  real<lower=0> i2 = square(tau) / (square(tau) + se2_hat); // proportion of total variance in effect size estimate due to heterogeneity between studies rather than sampling error
  vector[J] p = 1 - (square(tau) / (square(tau) + se2)); // proportion of variance in the trial_specific effect size estimate that is due to the true effect size rather than sampling error
  
  // posterior predictive distribution
  real theta_new = normal_rng(mu, tau); 
  
  // event probabilities 
  real mu_gt_0 = mu > 0;
  real theta_new_gt_0 = theta_new > 0;
  
  // Universe of 100 possible future studies
  array[100] real theta_100;
  for (i in 1:100) {
    theta_100[i] = normal_rng(mu, tau);
  }
}
