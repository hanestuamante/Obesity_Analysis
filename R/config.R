# Project-wide configuration -------------------------------------------------

PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

PATHS <- list(
  data_raw = file.path(PROJECT_DIR, "data", "raw_data.csv"),
  plots = file.path(PROJECT_DIR, "plots"),
  outputs = file.path(PROJECT_DIR, "outputs")
)

SEED <- 100

CONTINUOUS_VARS <- c("Age", "FCVC", "NCP", "CH2O", "FAF", "TUE", "BMI")

CATEGORICAL_VARS <- c(
  "Gender",
  "family_history_with_overweight",
  "FAVC",
  "CAEC",
  "SMOKE",
  "SCC",
  "CALC",
  "MTRANS"
)

MODEL_FORMULA <- NObeyesdad_group ~ Gender + Age + family_history_with_overweight +
  FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS

TARGET_LEVELS <- c("Normal", "Overweight", "Obesity")
