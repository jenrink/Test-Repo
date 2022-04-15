# R in VSCode --> Terminal --> Run Selected Text
# Cleaned-Up PSTAT 120B Calculations in R

# z-score value
qnorm(0.005, lower.tail = FALSE)    # Right-Tailed Test
qnorm(0.05, lower.tail = TRUE)      # Left-Tailed Test
2 * qnorm(0.01, lower.tail = FALSE)     # Two-Tailed Test

# t-distribution critical value
qt(0.05 / 2, df = 18, lower.tail = FALSE)
qt(0.025, df = 9, lower.tail = TRUE)

# x^2-distribution critical value
qchisq(1 - (0.1 / 2), df = 5, lower.tail = FALSE)
qchisq(0.95, df = 5, lower.tail = FALSE)

# Sp^2 Calculator ( Pooled Estimator )
n1 <- 11
n2 <- 14
s1 <- sqrt(52)
s2 <- sqrt(71)
pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n1 - 2))
pooled * pooled # this equals Sp not Sp^2

# Confidence Interval Calculator for mew
avg <- 98.25
std <- 0.73
z_score <- qnorm(0.005, lower.tail = FALSE)
size <- 130
std_size <- ((std) / (sqrt(size)))

lower_CI <- (avg - (z_score) * (std_size))
lower_CI
upper_CI <- (avg + (z_score) * (std_size))
upper_CI

# Confidence Interval Calculator for p hat
p_hat <- 0.45
z_score <- qnorm(0.01, lower.tail = FALSE)
size <- 800
  
lower_CI <- (p_hat - (z_score) * (sqrt((p_hat * (1 - p_hat)) / size)))
lower_CI
upper_CI <- (p_hat + (z_score) * (sqrt((p_hat * (1 - p_hat)) / size)))
upper_CI

# Confidence Intervals for difference in means

y_bar1 <- 1.65
y_bar2 <- 1.43
n1 <- 30
n2 <- 35
s1 <- 0.26
s2 <- 0.22
z_score <- qnorm(0.005, lower.tail = FALSE)

lower_CI <- (y_bar1 - y_bar2) - z_score * (sqrt((s1^2 / n1) + (s2^2 / n2)))
lower_CI
upper_CI <- (y_bar1 - y_bar2) + z_score * (sqrt((s1^2 / n1) + (s2^2 / n2)))
upper_CI

# Confidence Intervals for mew, SMALL SAMPLE

n <- 10
mew_not <- 4.85
sample_mew <- 3.781
sample_std <- 0.18095

t_observed <- ((sample_mew - mew_not) / (sample_std / sqrt(n)))
t_observed

t_critical_value <- qt(0.1, df = 18, lower.tail = FALSE)

lower_CI <- (sample_mew) - t_critical_value * (sample_std / sqrt(n))
lower_CI
upper_CI <- (sample_mew) + t_critical_value * (sample_std / sqrt(n))
upper_CI