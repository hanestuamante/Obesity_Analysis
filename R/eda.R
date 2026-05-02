# Exploratory data analysis --------------------------------------------------

plot_target_distribution <- function(data) {
  ggplot(data, aes(x = NObeyesdad_group, fill = NObeyesdad_group)) +
    geom_bar(color = "black", alpha = 0.9) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Distribution of Obesity Status",
      x = "Obesity status",
      y = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

plot_numeric_histogram <- function(data, variable, fill = "steelblue") {
  ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(fill = fill, color = "black", bins = 30) +
    labs(
      title = paste("Histogram of", variable),
      x = variable,
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
}

plot_categorical_composition <- function(data, variable) {
  ggplot(data, aes(x = .data[[variable]], fill = NObeyesdad_group)) +
    geom_bar(position = "fill", color = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = paste("Obesity Status by", variable),
      x = variable,
      y = "Share",
      fill = "Status"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot_continuous_boxplot <- function(data, variable) {
  ggplot(data, aes(x = NObeyesdad_group, y = .data[[variable]], fill = NObeyesdad_group)) +
    geom_boxplot(alpha = 0.85, outlier.alpha = 0.45) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = paste(variable, "by Obesity Status"),
      x = "Obesity status",
      y = variable
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

save_eda_plots <- function(data, plots_dir = PATHS$plots) {
  ggsave(file.path(plots_dir, "distribution.png"), plot_target_distribution(data), width = 8, height = 5, dpi = 300)
  ggsave(file.path(plots_dir, "age_histogram.png"), plot_numeric_histogram(data, "Age", "skyblue"), width = 8, height = 5, dpi = 300)
  ggsave(file.path(plots_dir, "ncp_histogram.png"), plot_numeric_histogram(data, "NCP", "lightgreen"), width = 8, height = 5, dpi = 300)

  for (var in CATEGORICAL_VARS) {
    file_name <- paste0("composition_", tolower(var), ".png")
    ggsave(file.path(plots_dir, file_name), plot_categorical_composition(data, var), width = 8, height = 5, dpi = 300)
  }

  for (var in CONTINUOUS_VARS) {
    file_name <- paste0("boxplot_", tolower(var), ".png")
    ggsave(file.path(plots_dir, file_name), plot_continuous_boxplot(data, var), width = 8, height = 5, dpi = 300)
  }
}

save_correlation_plot <- function(data, variables = CONTINUOUS_VARS, plots_dir = PATHS$plots) {
  cor_matrix <- cor(data[, variables], use = "complete.obs")

  png(file.path(plots_dir, "correlation_matrix.png"), width = 900, height = 900)
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 45
  )
  dev.off()

  cor_matrix
}
