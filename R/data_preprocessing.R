# Data loading and preprocessing --------------------------------------------

read_obesity_data <- function(path = PATHS$data_raw) {
  read.csv(path, stringsAsFactors = FALSE)
}

check_outliers <- function(data) {
  numeric_data <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]

  outlier_counts <- vapply(numeric_data, function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr_value <- q3 - q1
    sum(x < q1 - 1.5 * iqr_value | x > q3 + 1.5 * iqr_value, na.rm = TRUE)
  }, numeric(1))

  total_values <- vapply(numeric_data, function(x) sum(!is.na(x)), integer(1))

  data.frame(
    variable = names(outlier_counts),
    outliers = as.integer(outlier_counts),
    percent = round(outlier_counts / total_values * 100, 2),
    row.names = NULL
  )
}

replace_outliers_median <- function(x, round_result = FALSE) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value

  x[x < lower | x > upper] <- median(x, na.rm = TRUE)

  if (round_result) {
    x <- round(x)
  }

  x
}

prepare_obesity_data <- function(raw_data) {
  raw_data %>%
    mutate(
      across(
        c(
          Gender,
          family_history_with_overweight,
          FAVC,
          CAEC,
          SMOKE,
          SCC,
          CALC,
          MTRANS,
          NObeyesdad
        ),
        as.factor
      ),
      MTRANS = forcats::fct_collapse(MTRANS, Other = c("Bike", "Motorbike", "Walking")),
      CALC = forcats::fct_collapse(CALC, Often = c("Always", "Frequently")),
      CAEC = forcats::fct_collapse(CAEC, Often = c("Always", "Frequently")),
      Age = replace_outliers_median(Age, round_result = TRUE),
      BMI = Weight / (Height^2),
      NObeyesdad_group = case_when(
        NObeyesdad %in% c("Insufficient_Weight", "Normal_Weight") ~ "Normal",
        NObeyesdad %in% c("Overweight_Level_I", "Overweight_Level_II") ~ "Overweight",
        NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III") ~ "Obesity",
        TRUE ~ NA_character_
      ),
      NObeyesdad_group = factor(
        NObeyesdad_group,
        levels = TARGET_LEVELS,
        ordered = TRUE
      )
    ) %>%
    dplyr::select(-Weight, -Height)
}

summarise_continuous_by_target <- function(data, variables = CONTINUOUS_VARS) {
  data %>%
    group_by(NObeyesdad_group) %>%
    summarise(
      across(
        all_of(variables),
        list(
          min = ~ min(.x, na.rm = TRUE),
          q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          mean = ~ mean(.x, na.rm = TRUE),
          q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

summarise_categorical_by_target <- function(data, variables = CATEGORICAL_VARS) {
  lapply(variables, function(var) {
    as.data.frame.matrix(table(data[[var]], data$NObeyesdad_group)) %>%
      tibble::rownames_to_column(var = var)
  }) |>
    stats::setNames(variables)
}
