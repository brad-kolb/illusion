# experiment two
source("functions.r")
library(dplyr)
library(cmdstanr)

model <- cmdstan_model("model.stan")
n_sims <- 20

results_low <- lapply(1:n_sims, function(x) run_single_sim(seed = x, n_trials = 3))
results_mod <- lapply(1:n_sims, function(x) run_single_sim(seed = x, n_trials = 10))
results_high <-  lapply(1:n_sims, function(x) run_single_sim(seed = x, n_trials = 20))

probs_df <- rbind(
  cbind(do.call(rbind, lapply(results_low, function(x) x$probs)), condition="low"),
  cbind(do.call(rbind, lapply(results_mod, function(x) x$probs)), condition="moderate"), 
  cbind(do.call(rbind, lapply(results_high, function(x) x$probs)), condition="high")
) %>% as.data.frame()

probs_df$condition <- factor(probs_df$condition, 
                             levels = c("low", "moderate", "high"))

saveRDS(probs_df, "experiment_two.RDS")

pdf(here::here("doc/plots", "experiment_two.pdf"), width = 6, height = 6)
colors <- c("#E69F00", "#56B4E9", "#009E73", 
            "#F0E442", "#0072B2", "#D55E00", 
            "#CC79A7", "#000000")
par(mar = c(4, 4, 3, 2))
plot(probs_df$mu_gt_0, probs_df$theta_new_gt_0,
     xlab = expression(P(mu > 0)), 
     ylab = expression(P(theta[new] > 0)),
     main = "Average effect size vs. predicted effect size",
     col = colors[probs_df$condition],  
     pch = 19,
     cex = 0.75)
abline(0, 1, lty = 2)
abline(v = .975, lty = 3)
abline(h = .975, lty = 3)
legend("bottomright", 
       legend = c("Low n", "Moderate n", "High n"),
       col = colors,  
       pch = 19)

dev.off()