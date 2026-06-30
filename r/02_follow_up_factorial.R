# Chemical Process Yield Optimization
# Stage 2: Follow-up 2^3 factorial experiment
#
# Goal: Follow up on the initial screening results by evaluating
# factors B, D, and F while holding the remaining factors constant.

# ------------------------------------------------------------------------------
# 1. Enter Stage 2 design and yield data
# ------------------------------------------------------------------------------

follow_up_data <- data.frame(
  run = 1:8,
  
  B = c(7.5, 7.5, 7.5, 7.5, 10, 10, 10, 10),
  D = c(0, 0, 2.5, 2.5, 0, 0, 2.5, 2.5),
  F = c(0, 2.5, 0, 2.5, 0, 2.5, 0, 2.5),
  
  yield = c(
    86.29, 77.01, 60.38, 52.97,
    73.26, 68.84, 50.37, 45.05
  )
)

print(follow_up_data)

# ------------------------------------------------------------------------------
# 2. Code factor levels: low = -1, high = +1
# ------------------------------------------------------------------------------

coded_data <- data.frame(
  run = follow_up_data$run,
  yield = follow_up_data$yield,
  
  B = ifelse(follow_up_data$B == 7.5, -1, 1),
  D = ifelse(follow_up_data$D == 0, -1, 1),
  F = ifelse(follow_up_data$F == 0, -1, 1)
)

print(coded_data)

# ------------------------------------------------------------------------------
# 3. Estimate main effects and interactions
# ------------------------------------------------------------------------------

estimate_effect <- function(x, y) {
  sum(x * y) / (length(y) / 2)
}

effects <- c(
  B = estimate_effect(coded_data$B, coded_data$yield),
  D = estimate_effect(coded_data$D, coded_data$yield),
  F = estimate_effect(coded_data$F, coded_data$yield),
  
  `B:D` = estimate_effect(coded_data$B * coded_data$D, coded_data$yield),
  `B:F` = estimate_effect(coded_data$B * coded_data$F, coded_data$yield),
  `D:F` = estimate_effect(coded_data$D * coded_data$F, coded_data$yield),
  
  `B:D:F` = estimate_effect(
    coded_data$B * coded_data$D * coded_data$F,
    coded_data$yield
  )
)

effect_results <- data.frame(
  effect = names(effects),
  estimate = as.numeric(effects),
  absolute_effect = abs(as.numeric(effects))
)

effect_results <- effect_results[
  order(effect_results$absolute_effect, decreasing = TRUE),
]

print(effect_results)

# ------------------------------------------------------------------------------
# 4. Screen effects using Lenth's pseudo standard error
# ------------------------------------------------------------------------------

absolute_effects <- abs(effects)

s0 <- 1.5 * median(absolute_effects)

pse <- 1.5 * median(
  absolute_effects[absolute_effects <= 2.5 * s0]
)

margin_of_error <- 2.5 * pse
simultaneous_margin <- 3.07 * pse

lenth_results <- data.frame(
  effect = names(effects),
  estimate = as.numeric(effects),
  absolute_effect = absolute_effects,
  lenth_t = as.numeric(effects / pse),
  exceeds_margin_of_error = absolute_effects > margin_of_error,
  exceeds_simultaneous_margin = absolute_effects > simultaneous_margin
)

lenth_results <- lenth_results[
  order(lenth_results$absolute_effect, decreasing = TRUE),
]

print(lenth_results)

# ------------------------------------------------------------------------------
# 5. Create half-normal plot of effects
# ------------------------------------------------------------------------------

order_index <- order(abs(effects))

sorted_effects <- abs(effects)[order_index]
sorted_labels <- names(effects)[order_index]

n_effects <- length(sorted_effects)
plotting_positions <- ((1:n_effects) - 0.5) / n_effects
half_normal_quantiles <- qnorm((1 + plotting_positions) / 2)

plot(
  half_normal_quantiles,
  sorted_effects,
  xlab = "Half-normal quantile",
  ylab = "Absolute effect estimate",
  main = "Follow-Up Factorial: Half-Normal Plot of Effects"
)

text(
  half_normal_quantiles,
  sorted_effects,
  labels = sorted_labels,
  pos = 4,
  cex = 0.85
)

# ------------------------------------------------------------------------------
# 6. Fit first-order model in coded factor space
# ------------------------------------------------------------------------------

follow_up_model <- lm(
  yield ~ B + D + F,
  data = coded_data
)

summary(follow_up_model)
anova(follow_up_model)

# ------------------------------------------------------------------------------
# 7. Review diagnostics
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  fitted(follow_up_model),
  resid(follow_up_model),
  xlab = "Fitted yield",
  ylab = "Residuals",
  main = "Follow-Up Factorial: Residuals vs Fitted"
)

abline(h = 0, lty = 2)

qqnorm(
  resid(follow_up_model),
  main = "Follow-Up Factorial: Normal Q-Q Plot"
)

qqline(resid(follow_up_model))

par(mfrow = c(1, 1))