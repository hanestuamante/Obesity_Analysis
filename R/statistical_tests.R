# Inferential statistics -----------------------------------------------------

run_statistical_tests <- function(data, output_file = file.path(PATHS$outputs, "statistical_results.txt")) {
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  capture_section(check_outliers(data), "Outlier Summary", output_file)
  capture_section(summarise_continuous_by_target(data), "Continuous Variables by Obesity Status", output_file)

  for (var in CATEGORICAL_VARS) {
    capture_section(table(data[[var]], data$NObeyesdad_group), paste("Contingency Table:", var), output_file)
  }

  capture_section(by(data$BMI, data$NObeyesdad_group, shapiro.test), "Shapiro-Wilk Test for BMI by Group", output_file)
  capture_section(car::leveneTest(BMI ~ NObeyesdad_group, data = data), "Levene Test for Homogeneity of Variance", output_file)

  anova_model <- aov(BMI ~ NObeyesdad_group, data = data)
  capture_section(summary(anova_model), "One-way ANOVA: BMI by Obesity Status", output_file)
  capture_section(TukeyHSD(anova_model), "Tukey HSD Post-hoc Test", output_file)
  capture_section(kruskal.test(BMI ~ NObeyesdad_group, data = data), "Kruskal-Wallis Test", output_file)
  capture_section(dunn.test::dunn.test(data$BMI, data$NObeyesdad_group, method = "bonferroni"), "Dunn Post-hoc Test", output_file)

  invisible(
    list(
      anova_model = anova_model,
      output_file = output_file
    )
  )
}
