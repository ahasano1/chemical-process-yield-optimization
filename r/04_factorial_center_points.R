# Chemical Process Yield Optimization
# Stage 4: 2^3 factorial experiment with center-point replicates
#
# Goal: Study B, E, and G near the promising operating region,
# then assess whether curvature justifies response-surface modeling.
# All other factors were held constant.

# ------------------------------------------------------------------------------
# 1. Enter Stage 4 design and yield data
# ------------------------------------------------------------------------------

stage4_data <- data.frame(
  run = 1:12,
  
  B = c(4, 4, 6, 6, 4, 4, 6, 6, 5, 5, 5, 5),
  E = c(2.5, 2.5, 2.5, 2.5, 7.5, 7.5, 7.5, 7.5, 5, 5, 5, 5),
  G = c(2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 5, 5, 5, 5),
  
  yield = c(
    92.18, 90.81, 86.88, 89.84,
    88.96, 90.70, 87.62, 89.35,
    89.01, 91.33, 89.94, 89.87
  )
)

print(stage4_data)

# ------------------------------------------------------------------------------
# 2. Identify factorial runs and replicated center points
# ------------------------------------------------------------------------------

stage4_data$center_point <- with(
  stage4_data,
  as.integer(B == 5 & E == 5 & G == 5)
)

factorial_data <- subset(stage4_data, center_point == 0)
center_data <- subset(stage4_data, center_point == 1)

print(factorial_data)
print(center_data)

# ------------------------------------------------------------------------------
# 3. Code factors around the center point
# ------------------------------------------------------------------------------

stage4_data$B_coded <- (stage4_data$B - 5) / 1
stage4_data$E_coded <- (stage4_data$E - 5) / 2.5
stage4_data$G_coded <- (stage4_data$G - 5) / 2.5

factorial_data <- subset(stage4_data, center_point == 0)

# ------------------------------------------------------------------------------
# 4. Estimate factorial effects from the eight corner runs
# ------------------------------------------------------------------------------

estimate_effect <- function(x, y) {
  sum(x * y) / (length(y) / 2)
}

stage4_effects <- c(
  B = estimate_effect(factorial_data$B_coded, factorial_data$yield),
  E = estimate_effect(factorial_data$E_coded, factorial_data$yield),
  G = estimate_effect(factorial_data$G_coded, factorial_data$yield),
  
  `B:E` = estimate_effect(
    factorial_data$B_coded * factorial_data$E_coded,
    factorial_data$yield
  ),
  
  `B:G` = estimate_effect(
    factorial_data$B_coded * factorial_data$G_coded,
    factorial_data$yield
  ),
  
  `E:G` = estimate_effect(
    factorial_data$E_coded * factorial_data$G_coded,
    factorial_data$yield
  ),
  
  `B:E:G` = estimate_effect(
    factorial_data$B_coded *
      factorial_data$E_coded *
      factorial_data$G_coded,
    factorial_data$yield
  )
)

effect_results <- data.frame(
  effect = names(stage4_effects),
  estimate = as.numeric(stage4_effects),
  absolute_effect = abs(as.numeric(stage4_effects))
)

effect_results <- effect_results[
  order(effect_results$absolute_effect, decreasing = TRUE),
]

print(effect_results)

# ------------------------------------------------------------------------------
# 5. Compare center-point performance with factorial-run performance
# ------------------------------------------------------------------------------

center_summary <- data.frame(
  factorial_mean = mean(factorial_data$yield),
  center_mean = mean(center_data$yield),
  center_minus_factorial = mean(center_data$yield) - mean(factorial_data$yield),
  center_sd = sd(center_data$yield)
)

print(center_summary)

# ------------------------------------------------------------------------------
# 6. Fit a first-order model and test the center-point indicator
# ------------------------------------------------------------------------------

first_order_model <- lm(
  yield ~ B_coded + E_coded + G_coded,
  data = stage4_data
)

curvature_model <- lm(
  yield ~ B_coded + E_coded + G_coded + center_point,
  data = stage4_data
)

summary(first_order_model)
anova(first_order_model)

summary(curvature_model)
anova(curvature_model)

# ------------------------------------------------------------------------------
# 7. Review diagnostics for the center-point model
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  fitted(curvature_model),
  resid(curvature_model),
  xlab = "Fitted yield",
  ylab = "Residuals",
  main = "Stage 4: Residuals vs Fitted"
)

abline(h = 0, lty = 2)

qqnorm(
  resid(curvature_model),
  main = "Stage 4: Normal Q-Q Plot"
)

qqline(resid(curvature_model))

par(mfrow = c(1, 1))