# simulate
run_single_sim <- function(seed, 
                           n_trials = 10,
                           true_mu = 0.7, 
                           true_tau = 0.7,
                           n_range = c(200, 200)) {
  
  set.seed(seed)
  
  # Generate varying sample sizes
  n_per_trial <- round(runif(n_trials, n_range[1], n_range[2]))
  
  # Generate true effects for each trial
  true_effects <- rnorm(n_trials, true_mu, true_tau)
  
  # Generate trial data
  data <- list(
    J = n_trials,
    n_t = n_per_trial,
    r_t = rbinom(n_trials, n_per_trial, plogis(true_effects)),
    n_c = n_per_trial, 
    r_c = rbinom(n_trials, n_per_trial, plogis(0)),
    estimate_posterior = 1,
    priors = 0
  )
  
  observed_effects <-  log(data$r_t) - log(data$n_t - data$r_t) - 
    (log(data$r_c) - log(data$n_c - data$r_c))
  
  observed_se <- sqrt(1/data$r_t + 1/(data$n_t - data$r_t) 
                      + 1/data$r_c + 1/(data$n_c - data$r_c)
  )
  
  observed_z <- abs(observed_effects/observed_se)
  
  # Fit model
  fit <- model$sample(
    data = data,
    seed = seed,
    chains = 4,
    parallel_chains = 4,
    refresh = 0,
    init = 1,
    adapt_delta = 0.99
  )
  
  print(fit$diagnostic_summary())
  print(fit$summary(variables = c("mu", "tau"), "rhat", "ess_bulk"))
  
  # Extract posterior samples
  draws <- fit$draws()
  summ <- fit$summary()
  mu_samples <- as.vector(draws[,,"mu"])
  theta_new_samples <- as.vector(draws[,,"theta_new"])
  theta_100_samples <- fit$draws("theta_100", format = "matrix")
  
  # Calculate metrics
  coverage_mu <- mean(true_mu > quantile(mu_samples, 0.025) & 
                        true_mu < quantile(mu_samples, 0.975))
  coverage_theta <- mean(true_effects > quantile(theta_new_samples, 0.025) & 
                           true_effects < quantile(theta_new_samples, 0.975))
  
  # Return results
  list(
    observations = list(
      observed_effects = observed_effects,
      observed_se = observed_se,
      observed_z = observed_z
    ),
    true_effects = true_effects,
    diagnostics = list(
      rhat = max(summ$rhat, na.rm=TRUE),
      min_ess = min(summ$ess_bulk, na.rm=TRUE)
    ),
    coverage = list(
      mu = coverage_mu,
      theta = coverage_theta
    ),
    samples = list(
      mu = fit$draws("mu", format = "matrix"),
      theta_new = fit$draws("theta_new", format = "matrix"),
      theta_100 = fit$draws("theta_100", format = "matrix")
    ),
    probs = c(
      mu_gt_0 = mean(mu_samples > 0),
      theta_new_gt_0 = mean(theta_new_samples > 0)
    )
  )
}

# Aggregate results
summarize_results <- function(results) {
  coverage_mu <- mean(sapply(results, function(x) x$coverage$mu))
  coverage_theta <- mean(sapply(results, function(x) x$coverage$theta))
  prob_data <- lapply(results, function(x) {
    mu_prob <- x$probs[1]  # mu_gt_0
    theta_prob <- x$probs[2]  # theta_new_gt_0
    c(mu_prob, theta_prob, mu_prob - theta_prob)
  })
  prob_comparisons <- do.call(rbind, prob_data)
  colnames(prob_comparisons) <- c("mu", "theta", "diff")
  
  list(
    coverage = list(mu = coverage_mu, theta = coverage_theta),
    prob_summary = list(
      mean_diff = mean(prob_comparisons[,"diff"]),
      sd_diff = sd(prob_comparisons[,"diff"]),
      range_diff = range(prob_comparisons[,"diff"])
    ),
    prob_distributions = prob_comparisons
  )
}