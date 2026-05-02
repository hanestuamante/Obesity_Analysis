#!/usr/bin/env Rscript

command_args <- commandArgs(FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)

if (length(script_arg) == 1) {
  script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
  setwd(dirname(dirname(script_path)))
}

source(file.path("R", "utils.R"))
source(file.path("R", "config.R"))

load_packages(c(
  "dplyr",
  "ggplot2",
  "corrplot",
  "car",
  "FSA",
  "dunn.test",
  "MASS",
  "caret",
  "pROC",
  "lmtest",
  "forcats",
  "tibble",
  "scales"
))

source(file.path("R", "data_preprocessing.R"))
source(file.path("R", "eda.R"))
source(file.path("R", "statistical_tests.R"))
source(file.path("R", "modeling.R"))
source(file.path("R", "assumptions.R"))

create_project_dirs(PATHS)

raw_data <- read_obesity_data(PATHS$data_raw)
obesity <- prepare_obesity_data(raw_data)

saveRDS(obesity, file.path(PATHS$outputs, "clean_obesity_data.rds"))
write.csv(obesity, file.path(PATHS$outputs, "clean_obesity_data.csv"), row.names = FALSE)

save_eda_plots(obesity, PATHS$plots)
cor_matrix <- save_correlation_plot(obesity, CONTINUOUS_VARS, PATHS$plots)
write.csv(cor_matrix, file.path(PATHS$outputs, "correlation_matrix.csv"))

run_statistical_tests(obesity, file.path(PATHS$outputs, "statistical_results.txt"))

split <- split_train_test(obesity, p = 0.7, seed = SEED)
models <- fit_ordinal_models(split$train)

model_result <- evaluate_model(
  models$without_bmi,
  split$test,
  file.path(PATHS$outputs, "model_performance.txt")
)

auc_values <- save_roc_curves(split$test, model_result$probabilities, PATHS$plots)
write.csv(data.frame(class = names(auc_values), auc = as.numeric(auc_values)), file.path(PATHS$outputs, "auc_values.csv"), row.names = FALSE)

run_assumption_checks(models$without_bmi, split$train, file.path(PATHS$outputs, "assumptions_check.txt"))
save_log_odds_diagnostic(models$without_bmi, split$train, PATHS$plots)

saveRDS(models$without_bmi, file.path(PATHS$outputs, "ordinal_logistic_model.rds"))

message("Pipeline completed successfully.")
message("Plots saved to: ", PATHS$plots)
message("Outputs saved to: ", PATHS$outputs)
