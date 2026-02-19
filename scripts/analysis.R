library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(FSA)
library(dunn.test)
library(MASS)
library(caret)
library(pROC)
library(brant)
library(lmtest)

# --- 2. Load và Tiền xử lý dữ liệu ---
Obesity <- read.csv("data/raw_data.csv")

# Chuyển đổi factor và gộp nhóm bằng dplyr (Chuyên nghiệp hơn)
Obesity <- Obesity %>%
  mutate(across(c(Gender, family_history_with_overweight, FAVC, CAEC, SMOKE, SCC, CALC, MTRANS, NObeyesdad), as.factor)) %>%
  mutate(
    MTRANS = forcats::fct_collapse(MTRANS, Other = c("Bike", "Motorbike", "Walking")),
    CALC   = forcats::fct_collapse(CALC, Often = c("Always", "Frequently")),
    CAEC   = forcats::fct_collapse(CAEC, Often = c("Always", "Frequently")),
    BMI    = Weight / (Height^2)
  ) %>%
  mutate(NObeyesdad_group = case_when(
    NObeyesdad %in% c("Insufficient_Weight", "Normal_Weight") ~ "Normal",
    NObeyesdad %in% c("Overweight_Level_I", "Overweight_Level_II") ~ "Overweight",
    TRUE ~ "Obesity"
  )) %>%
  mutate(NObeyesdad_group = factor(NObeyesdad_group, levels = c("Normal", "Overweight", "Obesity"), ordered = TRUE))

# Loại bỏ cột thừa
Obesity <- Obesity %>% dplyr::select(-Weight, -Height)

# --- 3. Xử lý Outliers cho Age ---
replace_outliers_median <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- median(x, na.rm = TRUE)
  return(round(x))
}
Obesity$Age <- replace_outliers_median(Obesity$Age)

# --- 4. Exploratory Data Analysis (EDA) ---
# Vẽ và lưu biểu đồ
p1 <- ggplot(Obesity, aes(x = NObeyesdad_group, fill = NObeyesdad_group)) + 
  geom_bar() + theme_minimal() + labs(title = "Phân bố mức độ béo phì")
ggsave("plots/distribution.png", p1)

# Ma trận tương quan
png("plots/correlation_matrix.png", width=800, height=800)
num_vars <- c("Age","FCVC","NCP","CH2O","FAF","TUE","BMI")
cor_matrix <- cor(Obesity[, num_vars])
corrplot(cor_matrix, method="color", addCoef.col="black", type="upper")
dev.off()

# --- 5. Thống kê suy diễn (Inferential Statistics) ---
# ANOVA & Kruskal-Wallis cho BMI
anova_res <- aov(BMI ~ NObeyesdad_group, data = Obesity)
capture.output(summary(anova_res), file = "scripts/statistical_results.txt")
capture.output(kruskal.test(BMI ~ NObeyesdad_group, data = Obesity), file = "scripts/statistical_results.txt", append = TRUE)

# --- 6. Mô hình hồi quy Ordinal Logistic ---
set.seed(100)
train_index <- createDataPartition(Obesity$NObeyesdad_group, p = 0.7, list = FALSE)
train_data <- Obesity[train_index, ]
test_data <- Obesity[-train_index, ]

model2 <- polr(NObeyesdad_group ~ Gender + Age + family_history_with_overweight + 
               FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS, 
               data = train_data, Hess = TRUE)

# Đánh giá mô hình
pred_class <- predict(model2, newdata = test_data)
conf_mat <- confusionMatrix(pred_class, test_data$NObeyesdad_group)
capture.output(conf_mat, file = "scripts/model_performance.txt")

# ROC & AUC
pred_prob <- predict(model2, newdata = test_data, type = "probs")
png("plots/roc_curves.png", width=1200, height=400)
par(mfrow = c(1, 3))
for(lvl in levels(Obesity$NObeyesdad_group)) {
  roc_obj <- roc(ifelse(test_data$NObeyesdad_group == lvl, 1, 0), pred_prob[, lvl])
  plot(roc_obj, main = paste("ROC:", lvl), col = "blue")
  text(0.4, 0.2, paste("AUC =", round(auc(roc_obj), 3)))
}
dev.off()

# --- 7. Kiểm định giả định (Assumptions) ---
# Brant test
b_test <- brant(model2)
capture.output(b_test, file = "scripts/assumptions_check.txt")