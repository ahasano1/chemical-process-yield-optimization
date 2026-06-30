# Chemical Process Yield Optimization
# Stage 5: Axial runs for response-surface modeling
#
# Goal: Add six axial runs around B = 5, E = 5, and G = 5.
# Combined with Stage 4, these runs form a face-centered
# central composite design for the final quadratic model.

# ------------------------------------------------------------------------------
# 1. Enter axial-run settings and yield data
# ------------------------------------------------------------------------------

axial_data <- data.frame(
  run = 1:6,
  
  B = c(4, 6, 5, 5, 5, 5),
  E = c(5, 5, 2.5, 7.5, 5, 5),
  G = c(5, 5, 5, 5, 2.5, 7.5),
  
  yield = c(89.55, 90.51, 92.17, 92.67, 89.86, 91.03)
)

print(axial_data)

# ------------------------------------------------------------------------------
# 2. Convert settings to coded units around the design center
# ------------------------------------------------------------------------------

axial_data$xB <- (axial_data$B - 5) / 1
axial_data$xE <- (axial_data$E - 5) / 2.5
axial_data$xG <- (axial_data$G - 5) / 2.5

print(axial_data)

# ------------------------------------------------------------------------------
# 3. Summarize directional changes along each factor axis
# ------------------------------------------------------------------------------

axis_results <- data.frame(
  factor = c("B", "E", "G"),
  
  low_setting = c(4, 2.5, 2.5),
  high_setting = c(6, 7.5, 7.5),
  
  low_yield = c(89.55, 92.17, 89.86),
  high_yield = c(90.51, 92.67, 91.03)
)

axis_results$high_minus_low <- (
  axis_results$high_yield - axis_results$low_yield
)

print(axis_results)

# ------------------------------------------------------------------------------
# 4. Plot yield changes across each axial direction
# ------------------------------------------------------------------------------

par(mfrow = c(1, 3))

plot(
  axial_data$B[1:2],
  axial_data$yield[1:2],
  type = "b",
  xlab = "B setting",
  ylab = "Yield (%)",
  main = "Axial Runs: B"
)

plot(
  axial_data$E[3:4],
  axial_data$yield[3:4],
  type = "b",
  xlab = "E setting",
  ylab = "Yield (%)",
  main = "Axial Runs: E"
)

plot(
  axial_data$G[5:6],
  axial_data$yield[5:6],
  type = "b",
  xlab = "G setting",
  ylab = "Yield (%)",
  main = "Axial Runs: G"
)

par(mfrow = c(1, 1))