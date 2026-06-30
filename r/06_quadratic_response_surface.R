# Chemical Process Yield Optimization
# Stage 6: Quadratic response-surface model
#
# Goal: Combine the Stage 4 factorial-plus-center-point design with
# the Stage 5 axial runs to fit a quadratic model for B, E, and G.
# The model identifies a promising operating region for confirmation runs.

# ------------------------------------------------------------------------------
# 1. Enter Stage 4 factorial and center-point data
# ------------------------------------------------------------------------------

stage4_data <- data.frame(
  B = c(4, 4, 6, 6, 4, 4, 6, 6, 5, 5, 5, 5),
  E = c(2.5, 2.5, 2.5, 2.5, 7.5, 7.5, 7.5, 7.5, 5, 5, 5, 5),
  G = c(2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 2.5, 7.5, 5, 5, 5, 5),
  yield = c(
    92.18, 90.81, 86.88, 89.84,
    88.96, 90.70, 87.62, 89.35,
    89.01, 91.33, 89.94, 89.87
  )
)

# ------------------------------------------------------------------------------
# 2. Enter Stage 5 axial-run data
# ------------------------------------------------------------------------------

stage5_data <- data.frame(
  B = c(4, 6, 5, 5, 5, 5),
  E = c(5, 5, 2.5, 7.5, 5, 5),
  G = c(5, 5, 5, 5, 2.5, 7.5),
  yield = c(89.55, 90.51, 92.17, 92.67, 89.86, 91.03)
)

# ------------------------------------------------------------------------------
# 3. Combine both stages into a face-centered central composite design
# ------------------------------------------------------------------------------

ccd_data <- rbind(stage4_data, stage5_data)

ccd_data$xB <- (ccd_data$B - 5) / 1
ccd_data$xE <- (ccd_data$E - 5) / 2.5
ccd_data$xG <- (ccd_data$G - 5) / 2.5

print(ccd_data)
cat("Total central composite design runs:", nrow(ccd_data), "\n")

# ------------------------------------------------------------------------------
# 4. Compare first-order and full quadratic models
# ------------------------------------------------------------------------------

first_order_model <- lm(
  yield ~ xB + xE + xG,
  data = ccd_data
)

quadratic_model <- lm(
  yield ~ xB + xE + xG +
    I(xB^2) + I(xE^2) + I(xG^2) +
    xB:xE + xB:xG + xE:xG,
  data = ccd_data
)

cat("\nFirst-order model:\n")
print(summary(first_order_model))

cat("\nQuadratic response-surface model:\n")
print(summary(quadratic_model))

cat("\nDoes the quadratic model improve on the first-order model?\n")
print(anova(first_order_model, quadratic_model))

# ------------------------------------------------------------------------------
# 5. Calculate and classify the stationary point
# ------------------------------------------------------------------------------

model_coefficients <- coef(quadratic_model)

linear_terms <- c(
  model_coefficients["xB"],
  model_coefficients["xE"],
  model_coefficients["xG"]
)

hessian_matrix <- matrix(
  c(
    2 * model_coefficients["I(xB^2)"],
    model_coefficients["xB:xE"],
    model_coefficients["xB:xG"],
    
    model_coefficients["xB:xE"],
    2 * model_coefficients["I(xE^2)"],
    model_coefficients["xE:xG"],
    
    model_coefficients["xB:xG"],
    model_coefficients["xE:xG"],
    2 * model_coefficients["I(xG^2)"]
  ),
  nrow = 3,
  byrow = TRUE
)

stationary_point_coded <- -solve(hessian_matrix, linear_terms)

eigenvalues <- eigen(hessian_matrix, symmetric = TRUE)$values

stationary_classification <- if (all(eigenvalues < 0)) {
  "local maximum"
} else if (all(eigenvalues > 0)) {
  "local minimum"
} else {
  "saddle point"
}

stationary_point_natural <- c(
  B = 5 + stationary_point_coded[1] * 1,
  E = 5 + stationary_point_coded[2] * 2.5,
  G = 5 + stationary_point_coded[3] * 2.5
)

stationary_results <- data.frame(
  xB = stationary_point_coded[1],
  xE = stationary_point_coded[2],
  xG = stationary_point_coded[3],
  B = stationary_point_natural[1],
  E = stationary_point_natural[2],
  G = stationary_point_natural[3]
)

cat("\nStationary-point results:\n")
print(stationary_results)

cat("\nHessian eigenvalues:\n")
print(eigenvalues)

cat("\nStationary-point classification:", stationary_classification, "\n")

# ------------------------------------------------------------------------------
# 6. Evaluate the setting selected for confirmation runs
# ------------------------------------------------------------------------------

confirmation_setting <- data.frame(
  B = 5,
  E = 7.5,
  G = 5
)

confirmation_setting$xB <- (confirmation_setting$B - 5) / 1
confirmation_setting$xE <- (confirmation_setting$E - 5) / 2.5
confirmation_setting$xG <- (confirmation_setting$G - 5) / 2.5

confirmation_prediction <- predict(
  quadratic_model,
  newdata = confirmation_setting,
  interval = "confidence"
)

confirmation_results <- cbind(
  confirmation_setting,
  predicted_yield = confirmation_prediction
)

cat("\nModel prediction at the selected confirmation setting:\n")
print(confirmation_results)

# ------------------------------------------------------------------------------
# 7. Contour plot: B and E with G held at 5
# ------------------------------------------------------------------------------

B_values <- seq(4, 6, length.out = 75)
E_values <- seq(2.5, 7.5, length.out = 75)

prediction_grid <- expand.grid(
  B = B_values,
  E = E_values
)

prediction_grid$G <- 5
prediction_grid$xB <- (prediction_grid$B - 5) / 1
prediction_grid$xE <- (prediction_grid$E - 5) / 2.5
prediction_grid$xG <- (prediction_grid$G - 5) / 2.5

prediction_grid$predicted_yield <- predict(
  quadratic_model,
  newdata = prediction_grid
)

yield_surface <- matrix(
  prediction_grid$predicted_yield,
  nrow = length(B_values),
  ncol = length(E_values)
)

contour(
  x = B_values,
  y = E_values,
  z = yield_surface,
  xlab = "Factor B setting",
  ylab = "Factor E setting",
  main = "Predicted Yield Surface With G Fixed at 5"
)

points(
  x = confirmation_setting$B,
  y = confirmation_setting$E,
  pch = 19
)

text(
  x = confirmation_setting$B,
  y = confirmation_setting$E,
  labels = "Confirmation setting",
  pos = 4,
  cex = 0.8
)

# ------------------------------------------------------------------------------
# 8. Review quadratic-model diagnostics
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  fitted(quadratic_model),
  resid(quadratic_model),
  xlab = "Fitted yield",
  ylab = "Residuals",
  main = "Quadratic Model: Residuals vs Fitted"
)

abline(h = 0, lty = 2)

qqnorm(
  resid(quadratic_model),
  main = "Quadratic Model: Normal Q-Q Plot"
)

qqline(resid(quadratic_model))

par(mfrow = c(1, 1))