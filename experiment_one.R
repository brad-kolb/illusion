# experiment one
# meta-analysis of 10 trials, varying tau

source("functions.r")
library(dplyr)
library(cmdstanr)

model <- cmdstan_model("model.stan")
n_sims <- 20

results_low <- lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 0.35))
saveRDS(results_low, "experiment_one/results_low.RDS")
results_mod <- lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 0.7))
saveRDS(results_mod, "experiment_one/results_mod.RDS")
results_high <-  lapply(1:n_sims, function(x) run_single_sim(seed = x, true_tau = 1.4))
saveRDS(results_high, "experiment_one/results_high.RDS")


probs_df <- rbind(
  cbind(do.call(rbind, lapply(results_low, function(x) x$probs)), condition="low"),
  cbind(do.call(rbind, lapply(results_mod, function(x) x$probs)), condition="moderate"), 
  cbind(do.call(rbind, lapply(results_high, function(x) x$probs)), condition="high")
) %>% as.data.frame()

probs_df$condition <- factor(probs_df$condition, 
                             levels = c("low", "moderate", "high"))

saveRDS(probs_df, "experiment_one/probs.RDS")

#### results_low forests ####

mu_intervals <- lapply(results_low, function(x) {
  mu_samples <- x$samples$mu
  c(mean = mean(mu_samples),
    lower = quantile(mu_samples, 0.025),
    upper = quantile(mu_samples, 0.975))
}) %>%
  do.call(rbind, .) %>%
  as.data.frame() 


plot(NA, NA, 
     ylim = c(-1,1.75),
     xlim = c(0, nrow(mu_intervals)),
     xaxt = "n",
     ylab = "Effect Size",
     xlab = "Simulation",
     main = "Forest plot of μ posterior intervals")
# label first, middle and last simulation
axis(1, at=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)), 
     labels=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)))


abline(h = 0.7, lty = 2, col = "red")
abline(h = 0, lty = 2, col = "gray")

for(i in 1:nrow(mu_intervals)) {
  segments(i, mu_intervals$lower[i], i, mu_intervals$upper[i])
  points(i, mu_intervals$mean[i], pch = 20, cex = 1)
}

legend("bottomleft", 
       legend = c("Truth","Significance"),
       col = c("red","grey"),
       lty = 2,
       pt.cex = 0.7,
       bty = "n") 

#### forest 2 ####

# observed log odds ratios for each meta-analysis
y_observed <- lapply(results_low, function(x) {
  x$observations$observed_effects
})

y_true <- lapply(results_low, function(x) {
  x$true_effects
})

# common y-axis limits
y_limits <- c(-3, 3.5)

# forest plot for mu + observations
par(mar=c(4,4,4,2))
plot(NA, NA, 
     ylim = y_limits,
     xlim = c(0, nrow(mu_intervals)),
     xaxt = "n",
     ylab = "Effect Size",
     xlab = "Simulation",
     main = "Forest plot of μ posterior intervals")
# label first, middle and last simulation
axis(1, at=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)), 
     labels=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)))


abline(h = 0.7, lty = 2, col = "red")
abline(h = 0, lty = 2, col = "gray")

for(i in 1:nrow(mu_intervals)) {
  points(rep(i, length(y_observed[[i]])), y_observed[[i]], 
         col = adjustcolor("black", alpha=0.5), pch=1, cex=0.7)
  segments(i, mu_intervals$lower[i], i, mu_intervals$upper[i])
  points(i, mu_intervals$mean[i], pch = 20, cex = 1)
}

legend("bottomright", 
       legend = c("Observed", "Estimated"),
       col = c("black", "black"),
       pch = c(1, 20),
       lty = c(NA, 1),
       pt.cex = c(0.7, 0.9),
       bty = "n")  
legend("bottomleft", 
       legend = c("Truth","Significance"),
       col = c("red","grey"),
       lty = 2,
       pt.cex = 0.7,
       bty = "n") 

#### forest 3 ####

theta_new_intervals <- lapply(results_low, function(x) {
  theta_samples <- x$samples$theta_new
  c(mean = mean(theta_samples),
    lower = quantile(theta_samples, 0.025),
    upper = quantile(theta_samples, 0.975))
}) %>%
  do.call(rbind, .) %>%
  as.data.frame() 

plot(NA, NA, 
     ylim = y_limits,
     xlim = c(0, nrow(theta_new_intervals)),
     xaxt = "n",
     ylab = "Effect size",
     xlab = "Simulation",
     main = expression("Forest plot of " *theta[new]* " posterior intervals"))
axis(1, at=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)), 
     labels=c(1, floor(nrow(mu_intervals)/2), nrow(mu_intervals)))

abline(h = 0.7, lty = 2, col = "red")
abline(h = 0, lty = 2, col = "gray")

for(i in 1:nrow(theta_new_intervals)) {
  points(rep(i, length(y_observed[[i]])), y_observed[[i]], 
         col = adjustcolor("black", alpha=0.5), pch=1, cex=0.7)
  segments(i, theta_new_intervals$lower[i], 
           i, theta_new_intervals$upper[i])
  points(i, theta_new_intervals$mean[i], pch = 20, cex = 1)
}

legend("bottomright", 
       legend = c("Observed", "Estimated"),
       col = c("black", "black"),
       pch = c(1, 20),
       lty = c(NA, 1),
       pt.cex = c(0.7, 0.9),
       bty = "n") 
legend("bottomleft", 
       legend = c("Truth","Significance"),
       col = c("red","grey"),
       lty = 2,
       pt.cex = 0.7,
       bty = "n") 
