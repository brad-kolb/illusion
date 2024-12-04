# experiment one
source("functions.r")
library(dplyr)
library(cmdstanr)

model <- cmdstan_model("model.stan")
n_sims <- 20

results_low <- lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 0.35))
results_mod <- lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 0.7))
results_high <-  lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 1.4))

probs_df <- rbind(
  cbind(do.call(rbind, lapply(results_low, function(x) x$probs)), condition="low"),
  cbind(do.call(rbind, lapply(results_mod, function(x) x$probs)), condition="moderate"), 
  cbind(do.call(rbind, lapply(results_high, function(x) x$probs)), condition="high")
) %>% as.data.frame()

probs_df$condition <- factor(probs_df$condition, 
                             levels = c("low", "moderate", "high"))

saveRDS(probs_df, "experiment_one.RDS")

colors <- viridis::viridis(3)[c(1,2,3)] 
plot(probs_df$mu_gt_0, probs_df$theta_new_gt_0,
     xlab = expression(P(mu > 0)), 
     ylab = expression(P(theta[new] > 0)),
     main = "Average effect size vs. predicted effect size",
     col = colors[probs_df$condition],  
     pch = 19,
     cex = 0.5)
abline(0, 1, lty = 2)
abline(v = .975, lty = 3)
abline(h = .975, lty = 3)
legend("bottomright", 
       legend = c("Low τ", "Moderate τ", "High τ"),
       col = colors,  
       pch = 19)