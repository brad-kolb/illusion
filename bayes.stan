data {
  int<lower=1> studies;
  array[studies] real observed_effects;
  array[studies] real<lower=0> standard_errors;
}
transformed data {
  vector[studies] v = square(to_vector(standard_errors)); 
}
parameters {
  real mu;
  real<lower=0> tau;
  vector<offset=mu,multiplier=tau>[studies] true_effects;
}
model {
  observed_effects ~ normal(true_effects, standard_errors);
  true_effects ~ normal(mu, tau); 
  mu ~ std_normal();
  tau ~ cauchy(0, 0.5);
}
generated quantities {
  real<lower=0> i2 = square(tau) / (square(tau) + sum(v)/studies); 
  real new_true_effect = normal_rng(mu, tau); 
}

