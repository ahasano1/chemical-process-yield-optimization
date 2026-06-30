# Chemical Process Yield Optimization
# Stage 1: Initial 16-run fractional-factorial screening
#
# Goal: Screen eight process factors and identify candidate effects
# for follow-up experimentation.
#
# Note: Because this is a fractional-factorial design, effects should
# be interpreted as aliased effect estimates during screening.

# ------------------------------------------------------------------------------
# 1. Enter Stage 1 design and yield data
# ------------------------------------------------------------------------------

screening_data <- data.frame(
  run = 1:16,
  
  A = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
        7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5),
  
  B = c(2.5, 2.5, 2.5, 2.5, 7.5, 7.5, 7.5, 7.5,
        2.5, 2.5, 2.5, 2.5, 7.5, 7.5, 7.5, 7.5),
  
  C = c(2.5, 2.5, 7.5, 7.5, 2.5, 2.5, 7.5, 7.5,
        2.5, 2.5, 7.5, 7.5, 2.5, 2.5, 7.5, 7.5),
  
  D = c(2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 2.5, 7.5,
        2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 2.5, 7.5),
  
  E = c(2.5, 2.5, 7.5, 7.5, 7.5, 7.5, 2.5, 2.5,
        7.5, 7.5, 2.5, 2.5, 2.5, 2.5, 7.5, 7.5),
  
  F = c(2.5, 7.5, 2.5, 7.5, 7.5, 2.5, 7.5, 2.5,
        7.5, 2.5, 7.5, 2.5, 2.5, 7.5, 2.5, 7.5),
  
  G = c(2.5, 7.5, 7.5, 2.5, 2.5, 7.5, 7.5, 2.5,
        7.5, 2.5, 2.5, 7.5, 7.5, 2.5, 2.5, 7.5),
  
  H = c(2.5, 7.5, 7.5, 2.5, 7.5, 2.5, 2.5, 7.5,
        2.5, 7.5, 7.5, 2.5, 7.5, 2.5, 2.5, 7.5),
  
  yield = c(
    50.34, 30.35, 49.34, 26.21,
    53.75, 46.97, 55.09, 47.70,
    34.24, 41.55, 33.90, 42.27,
    52.14, 44.15, 52.99, 45.48
  )
)

print(screening_data)

# ------------------------------------------------------------------------------
# 2. Convert natural factor levels to coded values: low = -1, high = +1
# ------------------------------------------------------------------------------

factor_names <- LETTERS[1:8]

coded_factors <- as.data.frame(
  lapply(screening_data[factor_names], function(x) ifelse(x == 2.5, -1, 1))
)

names(coded_factors) <- factor_names

analysis_data <- cbind(
  screening_data[c("run", "yield")],
  coded_factors
)

print(analysis_data)

# ------------------------------------------------------------------------------
# 3. Verify fractional-factorial generator relationships
# ------------------------------------------------------------------------------

generator_checks <- c(
  E_equals_ABC = all(coded_factors$E == coded_factors$A * coded_factors$B * coded_factors$C),
  F_equals_ABD = all(coded_factors$F == coded_factors$A * coded_factors$B * coded_factors$D),
  G_equals_ACD = all(coded_factors$G == coded_factors$A * coded_factors$C * coded_factors$D),
  H_equals_BCD = all(coded_factors$H == coded_factors$B * coded_factors$C * coded_factors$D)
)

print(generator_checks)

# ------------------------------------------------------------------------------
# 4. Estimate main effects and two-factor interaction effects
# ------------------------------------------------------------------------------

estimate_effect <- function(x, y) {
  sum(x * y) / (length(y) / 2)
}

main_effects <- sapply(
  coded_factors,
  estimate_effect,
  y = analysis_data$yield
)

factor_pairs <- combn(factor_names, 2, simplify = FALSE)

two_factor_effects <- sapply(
  factor_pairs,
  function(pair) {
    estimate_effect(
      coded_factors[[pair[1]]] * coded_factors[[pair[2]]],
      analysis_data$yield
    )
  }
)

names(two_factor_effects) <- sapply(
  factor_pairs,
  paste,
  collapse = ":"
)

candidate_effects <- c(main_effects, two_factor_effects)

effect_results <- data.frame(
  effect = names(candidate_effects),
  estimate = as.numeric(candidate_effects),
  absolute_effect = abs(as.numeric(candidate_effects))
)

effect_results <- effect_results[
  order(effect_results$absolute_effect, decreasing = TRUE),
]

print(effect_results)

# ------------------------------------------------------------------------------
# 5. Screen candidate effects using Lenth's pseudo standard error
# ------------------------------------------------------------------------------

lenth_screen <- function(effects) {
  absolute_effects <- abs(effects)
  
  s0 <- 1.5 * median(absolute_effects)
  
  pse <- 1.5 * median(
    absolute_effects[absolute_effects <= 2.5 * s0]
  )
  
  data.frame(
    effect = names(effects),
    estimate = as.numeric(effects),
    absolute_effect = absolute_effects,
    lenth_t = as.numeric(effects / pse),
    exceeds_margin_of_error = absolute_effects > 2.5 * pse,
    exceeds_simultaneous_margin = absolute_effects > 3.5 * pse
  )
}

lenth_results <- lenth_screen(candidate_effects)

lenth_results <- lenth_results[
  order(lenth_results$absolute_effect, decreasing = TRUE),
]

print(lenth_results)

# ------------------------------------------------------------------------------
# 6. Create half-normal plot of candidate effects
# ------------------------------------------------------------------------------

plot_half_normal <- function(effects) {
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
    main = "Initial Screening: Half-Normal Plot of Effects"
  )
  
  text(
    half_normal_quantiles,
    sorted_effects,
    labels = sorted_labels,
    pos = 4,
    cex = 0.7
  )
}

plot_half_normal(candidate_effects)

# ------------------------------------------------------------------------------
# 7. Fit a first-order screening model in coded factor space
# ------------------------------------------------------------------------------

screening_model <- lm(
  yield ~ A + B + C + D + E + F + G + H,
  data = analysis_data
)

summary(screening_model)
anova(screening_model)

# ------------------------------------------------------------------------------
# 8. Review model diagnostics
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  fitted(screening_model),
  resid(screening_model),
  xlab = "Fitted yield",
  ylab = "Residuals",
  main = "Initial Screening: Residuals vs Fitted"
)

abline(h = 0, lty = 2)

qqnorm(
  resid(screening_model),
  main = "Initial Screening: Normal Q-Q Plot"
)

qqline(resid(screening_model))

par(mfrow = c(1, 1))