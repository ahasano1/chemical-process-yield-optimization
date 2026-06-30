# Chemical Process Yield Optimization
# Stage 7: Confirmation runs at the selected operating setting
#
# Goal: Validate the promising operating setting identified through
# the response-surface study with 12 independent confirmation runs.
#
# Selected setting:
# B = 5, E = 7.5, G = 5
# All other factors held constant.

# ------------------------------------------------------------------------------
# 1. Enter confirmation-run yields
# ------------------------------------------------------------------------------

confirmation_yield <- c(
  89.26, 92.43, 91.99, 92.04,
  90.41, 88.93, 88.66, 91.80,
  90.22, 91.17, 93.26, 90.39
)

confirmation_data <- data.frame(
  run = seq_along(confirmation_yield),
  B = 5,
  E = 7.5,
  G = 5,
  yield = confirmation_yield
)

print(confirmation_data)

# ------------------------------------------------------------------------------
# 2. Summarize confirmation-run performance
# ------------------------------------------------------------------------------

n_runs <- nrow(confirmation_data)
mean_yield <- mean(confirmation_data$yield)
sd_yield <- sd(confirmation_data$yield)
min_yield <- min(confirmation_data$yield)
max_yield <- max(confirmation_data$yield)

confirmation_summary <- data.frame(
  runs = n_runs,
  mean_yield = mean_yield,
  sd_yield = sd_yield,
  min_yield = min_yield,
  max_yield = max_yield
)

print(confirmation_summary)

# ------------------------------------------------------------------------------
# 3. Calculate a 95% confidence interval and prediction interval
# ------------------------------------------------------------------------------

degrees_freedom <- n_runs - 1
critical_t <- qt(0.975, df = degrees_freedom)

confidence_half_width <- critical_t * sd_yield / sqrt(n_runs)

prediction_half_width <- critical_t * sd_yield * sqrt(1 + 1 / n_runs)

confidence_interval <- c(
  lower = mean_yield - confidence_half_width,
  upper = mean_yield + confidence_half_width
)

prediction_interval <- c(
  lower = mean_yield - prediction_half_width,
  upper = mean_yield + prediction_half_width
)

cat("\n95% confidence interval for mean yield:\n")
print(confidence_interval)

cat("\n95% prediction interval for one future run:\n")
print(prediction_interval)

# ------------------------------------------------------------------------------
# 4. Compare confirmation performance with Stage 4 center-point runs
# ------------------------------------------------------------------------------

center_point_yield <- c(89.01, 91.33, 89.94, 89.87)

center_comparison <- data.frame(
  setting = c(
    "Stage 4 center point: B=5, E=5, G=5",
    "Confirmation setting: B=5, E=7.5, G=5"
  ),
  mean_yield = c(
    mean(center_point_yield),
    mean(confirmation_yield)
  ),
  sd_yield = c(
    sd(center_point_yield),
    sd(confirmation_yield)
  )
)

center_comparison$mean_difference_from_center <- (
  center_comparison$mean_yield - center_comparison$mean_yield[1]
)

print(center_comparison)

# ------------------------------------------------------------------------------
# 5. Check run-order stability
# ------------------------------------------------------------------------------

run_order_model <- lm(
  yield ~ run,
  data = confirmation_data
)

summary(run_order_model)

plot(
  confirmation_data$run,
  confirmation_data$yield,
  type = "b",
  xlab = "Confirmation run order",
  ylab = "Yield (%)",
  main = "Confirmation Runs at B=5, E=7.5, G=5"
)

abline(h = mean_yield, lty = 2)

# ------------------------------------------------------------------------------
# 6. Review distribution of confirmation yields
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

hist(
  confirmation_data$yield,
  xlab = "Yield (%)",
  main = "Confirmation-Run Yield Distribution"
)

qqnorm(
  confirmation_data$yield,
  main = "Confirmation Runs: Normal Q-Q Plot"
)

qqline(confirmation_data$yield)

par(mfrow = c(1, 1))