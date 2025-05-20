## Post-hoc power analysis 

library(pwr)

effect_size <- 0.30  # Desired effect size (30% difference)
alpha <- 0.05       # Significance level (typically 0.05)
power <- 0.8       # Desired power (typically 0.8 but 0.9 for higher power)
# n <- 172/2          # Actual number of surgeries 
power_analysis <- pwr.t.test(d = effect_size, alternative = "two.sided", sig.level = alpha, power = power)

sample_size <- ceiling(power_analysis$n)
sample_size_per_group <- ceiling(power_analysis$n / 2)

cat("Required Sample Size per Group:", sample_size_per_group, "\n")
cat("Required Sample Size:", sample_size, "\n")

