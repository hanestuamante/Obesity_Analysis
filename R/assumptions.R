# Model assumption checks ----------------------------------------------------

run_assumption_checks <- function(model, train_data, output_file = file.path(PATHS$outputs, "assumptions_check.txt")) {
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  vif_check_model <- lm(
    Age ~ Gender + family_history_with_overweight +
      FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC +
      FAF + TUE + CALC + MTRANS,
    data = train_data
  )

  capture_section(car::vif(vif_check_model), "Variance Inflation Factor", output_file)

  if (requireNamespace("brant", quietly = TRUE)) {
    capture_section(brant::brant(model), "Brant Test", output_file)
  } else {
    append_section("Brant Test", output_file)
    cat("Package 'brant' is not installed; skipping Brant test.\n", file = output_file, append = TRUE)
  }

  dw_check_model <- lm(
    as.numeric(NObeyesdad_group) ~ Age + family_history_with_overweight +
      FAVC + FCVC + NCP + CAEC + CH2O + FAF + TUE + CALC + MTRANS,
    data = train_data
  )

  capture_section(lmtest::dwtest(dw_check_model), "Durbin-Watson Test", output_file)

  invisible(output_file)
}

save_log_odds_diagnostic <- function(model, train_data, plots_dir = PATHS$plots) {
  probs_train <- predict(model, type = "probs")
  prob_normal <- probs_train[, "Normal"]
  log_odds_normal <- log(prob_normal / (1 - prob_normal))

  prob_cumulative_overweight <- probs_train[, "Normal"] + probs_train[, "Overweight"]
  log_odds_threshold2 <- log(prob_cumulative_overweight / (1 - prob_cumulative_overweight))

  png(file.path(plots_dir, "log_odds_diagnostic.png"), width = 1000, height = 500)
  par(mfrow = c(1, 2))

  plot(
    train_data$Age,
    log_odds_normal,
    main = "Threshold 1: Normal vs Others",
    xlab = "Age",
    ylab = "Log-odds",
    pch = 19,
    col = "blue",
    cex = 0.5
  )
  lines(lowess(train_data$Age, log_odds_normal), col = "red", lwd = 2)

  plot(
    train_data$Age,
    log_odds_threshold2,
    main = "Threshold 2: Non-obesity vs Obesity",
    xlab = "Age",
    ylab = "Log-odds",
    pch = 19,
    col = "darkgreen",
    cex = 0.5
  )
  lines(lowess(train_data$Age, log_odds_threshold2), col = "red", lwd = 2)

  par(mfrow = c(1, 1))
  dev.off()
}
