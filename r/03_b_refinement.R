# Chemical Process Yield Optimization
# Stage 3: B-level refinement experiment
#
# Goal: Compare B = 5.0 versus B = 7.5 after Stage 2 indicated
# that factor B had a meaningful effect on yield.
# All other factors were held constant.

# ------------------------------------------------------------------------------
# 1. Enter experiment settings and yield data
# ------------------------------------------------------------------------------

b_refinement_data <- data.frame(
  run = 1:4,
  B = c(5.0, 7.5, 5.0, 7.5),
  yield = c(91.61, 85.51, 88.30, 86.68)
)

print(b_refinement_data)

# ------------------------------------------------------------------------------
# 2. Compare yields at the two B settings
# ------------------------------------------------------------------------------

yield_summary <- aggregate(
  yield ~ B,
  data = b_refinement_data,
  FUN = function(x) {
    c(
      mean = mean(x),
      sd = sd(x),
      n = length(x)
    )
  }
)

print(yield_summary)

mean_low <- mean(b_refinement_data$yield[b_refinement_data$B == 5.0])
mean_high <- mean(b_refinement_data$yield[b_refinement_data$B == 7.5])

b_effect <- mean_high - mean_low

comparison_results <- data.frame(
  B_low_mean = mean_low,
  B_high_mean = mean_high,
  high_minus_low_effect = b_effect
)

print(comparison_results)

# ------------------------------------------------------------------------------
# 3. Fit a simple model for the B-level comparison
# ------------------------------------------------------------------------------

b_refinement_data$B_coded <- ifelse(
  b_refinement_data$B == 5.0,
  -1,
  1
)

b_refinement_model <- lm(
  yield ~ B_coded,
  data = b_refinement_data
)

summary(b_refinement_model)
anova(b_refinement_model)

# ------------------------------------------------------------------------------
# 4. Visualize yield at each B setting
# ------------------------------------------------------------------------------

stripchart(
  yield ~ factor(B),
  data = b_refinement_data,
  vertical = TRUE,
  method = "jitter",
  pch = 19,
  xlab = "Factor B setting",
  ylab = "Yield (%)",
  main = "B-Level Refinement: Yield by Factor B Setting"
)

points(
  x = c(1, 2),
  y = c(mean_low, mean_high),
  pch = 4,
  cex = 1.8,
  lwd = 2
)

legend(
  "topright",
  legend = "Mean yield",
  pch = 4,
  bty = "n"
)