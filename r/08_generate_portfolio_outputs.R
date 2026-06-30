# Generate portfolio-ready figures and a results summary table

source("r/06_quadratic_response_surface.R")
source("r/07_confirmation_runs.R")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------

# 1. Predicted-yield contour plot

# ------------------------------------------------------------------------------

png(
  filename = "outputs/figures/predicted_yield_contours.png",
  width = 1600,
  height = 1200,
  res = 200
)

par(mar = c(5, 4.5, 4.5, 2) + 0.1)

contour(
  x = B_values,
  y = E_values,
  z = yield_surface,
  xlab = "Factor B setting",
  ylab = "Factor E setting",
  ylim = c(2.3, 8),
  main = "Predicted Yield Contours With G Fixed at 5",
  sub = "Black point: selected confirmation setting, B = 5, E = 7.5"
)

points(
  x = confirmation_setting$B,
  y = confirmation_setting$E,
  pch = 19,
  cex = 1.2
)

dev.off()

# ------------------------------------------------------------------------------

# 2. Confirmation-run chart

# ------------------------------------------------------------------------------

png(
  filename = "outputs/figures/confirmation_runs.png",
  width = 1600,
  height = 1000,
  res = 200
)

plot(
  confirmation_data$run,
  confirmation_data$yield,
  type = "b",
  xlab = "Confirmation run order",
  ylab = "Yield (%)",
  main = "Confirmation Runs at B = 5, E = 7.5, G = 5"
)

abline(h = mean_yield, lty = 2)

legend(
  "bottomright",
  legend = paste("Mean yield =", round(mean_yield, 2), "%"),
  lty = 2,
  bty = "n"
)

dev.off()

# ------------------------------------------------------------------------------

# 3. Key-results table

# ------------------------------------------------------------------------------

key_results <- data.frame(
  metric = c(
    "Experimental-run budget",
    "Experimental runs used",
    "Selected confirmation setting",
    "Quadratic-model predicted yield",
    "Confirmation-run mean yield",
    "95% confidence interval for mean yield",
    "95% prediction interval for one future run"
  ),
  result = c(
    "75 runs",
    "58 runs",
    "B = 5, E = 7.5, G = 5",
    paste0(round(confirmation_prediction[1, "fit"], 2), "%"),
    paste0(round(mean_yield, 2), "%"),
    paste0(
      round(confidence_interval["lower"], 2),
      "% to ",
      round(confidence_interval["upper"], 2),
      "%"
    ),
    paste0(
      round(prediction_interval["lower"], 2),
      "% to ",
      round(prediction_interval["upper"], 2),
      "%"
    )
  )
)

write.csv(
  key_results,
  "outputs/tables/key_results.csv",
  row.names = FALSE
)

print(key_results)
