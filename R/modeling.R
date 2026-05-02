# Ordinal logistic modeling --------------------------------------------------

split_train_test <- function(data, p = 0.7, seed = SEED) {
  set.seed(seed)
  train_index <- caret::createDataPartition(data$NObeyesdad_group, p = p, list = FALSE)

  list(
    train = data[train_index, ],
    test = data[-train_index, ]
  )
}

fit_ordinal_models <- function(train_data) {
  model_with_bmi <- MASS::polr(
    NObeyesdad_group ~ Gender + Age + family_history_with_overweight +
      FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS + BMI,
    data = train_data,
    Hess = TRUE
  )

  model_without_bmi <- MASS::polr(
    MODEL_FORMULA,
    data = train_data,
    Hess = TRUE
  )

  list(
    with_bmi = model_with_bmi,
    without_bmi = model_without_bmi
  )
}

evaluate_model <- function(model, test_data, output_file = file.path(PATHS$outputs, "model_performance.txt")) {
  pred_class <- predict(model, newdata = test_data)
  pred_prob <- predict(model, newdata = test_data, type = "probs")
  conf_mat <- caret::confusionMatrix(pred_class, test_data$NObeyesdad_group)

  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  capture_section(summary(model), "Ordinal Logistic Regression Summary", output_file)
  capture_section(conf_mat, "Confusion Matrix", output_file)

  invisible(
    list(
      predictions = pred_class,
      probabilities = pred_prob,
      confusion_matrix = conf_mat,
      output_file = output_file
    )
  )
}

save_roc_curves <- function(test_data, pred_prob, plots_dir = PATHS$plots) {
  auc_values <- numeric(length(TARGET_LEVELS))
  names(auc_values) <- TARGET_LEVELS

  png(file.path(plots_dir, "roc_curves.png"), width = 1200, height = 400)
  par(mfrow = c(1, 3))

  for (level in TARGET_LEVELS) {
    roc_obj <- pROC::roc(
      response = ifelse(test_data$NObeyesdad_group == level, 1, 0),
      predictor = pred_prob[, level],
      quiet = TRUE
    )
    auc_values[level] <- as.numeric(pROC::auc(roc_obj))
    plot(roc_obj, main = paste("ROC:", level), col = "blue")
    text(0.4, 0.2, paste("AUC =", round(auc_values[level], 3)), col = "blue")
  }

  par(mfrow = c(1, 1))
  dev.off()

  auc_values
}
