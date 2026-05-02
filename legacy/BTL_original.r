library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(FSA)
library(dunn.test)
library(MASS)    # chứa polr() cho ordinal logistic
library(caret)   # confusionMatrix, chia dữ liệu

Obesity <- read.csv("~/Downloads/ObesityDataSet_raw_and_data_sinthetic.csv")
View(Obesity)

# Chuyển các biến phân loại (categorical) sang factor
Obesity <- Obesity %>%
  mutate(across(c(Gender,
                  family_history_with_overweight,
                  FAVC,
                  CAEC,
                  SMOKE,
                  SCC,
                  CALC,
                  MTRANS,
                  NObeyesdad), as.factor))

str(Obesity)

#Tạo hàm kiểm tra ngoại lai 
check_outliers <- function(data) {
  num <- data[, sapply(data, is.numeric), drop = FALSE]  # chỉ lấy cột số
  out <- sapply(num, function(x) {
    Q1 <- quantile(x, 0.25)  # phần tư thứ nhất
    Q3 <- quantile(x, 0.75)  # phần tư thứ ba
    IQR <- Q3 - Q1                         # khoảng IQR
    sum(x < Q1 - 1.5*IQR | x > Q3 + 1.5*IQR)  # đếm ngoại lai
  })
  
  total <- sapply(num, function(x) {sum(!is.na(x)}))         # tổng số giá trị
  percent <- round(out / total * 100, 2)                   # tỷ lệ %
  
  data.frame(outliers = out, percent = percent)
}

#Kiểm tra ngoại lai trong tập tin
check_outliers(Obesity)

#Kiểm tra phân bố của 2 biến có ngoại lai.
# Histogram của Age
ggplot(Obesity, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram của Age",
       x = "Age",
       y = "Tần suất") +
  theme_minimal()

#nhận xét: Biểu đồ histogram cho thấy phân bố lệch phải (right-skewed), cho thấy
#phần lớn người trong mẫu nằm ở nhóm tuổi trẻ (khoảng 18–25 tuổi), trong khi số lượng người lớn tuổi giảm dần.
#Kết luận: phân bố không chuẩn, có xu hướng lệch phải.

# Histogram của NCP
ggplot(Obesity, aes(x = NCP)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "Histogram của NCP",
       x = "NCP (Số bữa chính mỗi ngày)",
       y = "Tần suất") +
  theme_minimal()

#Biểu đồ histogram cho thấy giá trị tập trung cao nhất ở mức 3 bữa/ngày, 
#thể hiện thói quen ăn uống phổ biến.
#dữ liệu NCP có phân bố lệch nhẹ, nhưng không chuẩn,chủ  yếu tập trung tại một mức duy nhất (mode = 3)

#Hướng xử lý => thay thế trung vị của Age, 
#còn NCP sẽ giữ nguyên vì biến rời rạc (nhiều giá trị 3)

#Xử lý ngoại lai
replace_outliers_median <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  med <- median(x, na.rm = TRUE)
  
  x[x < lower | x > upper] <- med
  return(x)
}

# Áp dụng cho Age có ngoại lai
Obesity$Age <- replace_outliers_median(Obesity$Age)
Obesity$Age <- round(Obesity$Age)

# Tạo biến BMI
Obesity <- Obesity %>%
  mutate(BMI = Weight / (Height^2))

# Loại bỏ biến gốc Weight và Height nếu muốn
# Loại bỏ cột Weight và Height
Obesity <- Obesity[, !(names(Obesity) %in% c("Weight","Height"))]


str(Obesity)

#Tính thống kê mô tả cho các biến liên tục

# Biến số cần thống kê
num_vars <- c("Age","FCVC","NCP","CH2O","FAF","TUE","BMI")

# Hàm thống kê mô tả (trả về numeric)
des_func <- function(x) {
  sapply(x, function(y) c(
    Min = min(y, na.rm = TRUE),
    Q1 = quantile(y, 0.25, na.rm = TRUE),
    Median = median(y, na.rm = TRUE),
    Mean = mean(y, na.rm = TRUE),
    Q3 = quantile(y, 0.75, na.rm = TRUE),
    Max = max(y, na.rm = TRUE),
    SD = sd(y, na.rm = TRUE)
  ))
}

# Áp dụng theo nhóm NObeyesdad
by(Obesity[, num_vars], Obesity$NObeyesdad, des_func)

# Danh sách biến phân loại
categorical_vars <- c("Gender","family_history_with_overweight","FAVC",
                      "CAEC","SMOKE","SCC","CALC","MTRANS")

# Lặp qua từng biến
for (var in categorical_vars) {
  cat("Biến:", var)
  print(table(Obesity[[var]]))
  cat("\n")
}



# MTRANS: Bike + Motorbike + Walking-> Other
Obesity$MTRANS <- as.character(Obesity$MTRANS)
Obesity$MTRANS[Obesity$MTRANS %in% c("Bike","Motorbike","Walking")] <- "Other"
Obesity$MTRANS <- factor(Obesity$MTRANS)

# CALC: Always + Frequently -> Often
Obesity$CALC <- as.character(Obesity$CALC)
Obesity$CALC[Obesity$CALC %in% c("Always","Frequently")] <- "Often"
Obesity$CALC <- factor(Obesity$CALC)

# CAEC: Always + Frequently -> Often
Obesity$CAEC <- as.character(Obesity$CAEC)
Obesity$CAEC[Obesity$CAEC %in% c("Always","Frequently")] <- "Often"
Obesity$CAEC <- factor(Obesity$CAEC)

# Lặp qua từng biến
for (var in categorical_vars) {
  cat("Biến:", var)
  print(table(Obesity[[var]], Obesity$NObeyesdad))
  cat("\n")
}

# Gộp nhóm
Obesity$NObeyesdad_group[Obesity$NObeyesdad %in% c("Overweight_Level_I","Overweight_Level_II")] <- "Overweight"
Obesity$NObeyesdad_group[Obesity$NObeyesdad %in% c("Obesity_Type_I","Obesity_Type_II","Obesity_Type_III")] <- "Obesity"
Obesity$NObeyesdad_group[Obesity$NObeyesdad %in% c("Insufficient_Weight", "Normal_Weight")] <- "Normal"

# Chuyển thành ordered factor
Obesity$NObeyesdad_group <- factor(Obesity$NObeyesdad_group,
                                         levels = c("Normal","Overweight","Obesity"),
                                         ordered = TRUE)

# Lặp qua từng biến
for (var in categorical_vars) {
  cat("Biến:", var)
  print(table(Obesity[[var]], Obesity$NObeyesdad_group))
  cat("\n")
}

# Kiểm tra tần số sau gộp
table(Obesity$NObeyesdad_group)

library(ggplot2)

ggplot(Obesity, aes(x = NObeyesdad_group)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Tần số các mức độ béo phì (NObeyesdad)",
       x = "Tình trạng cơ thể",
       y = "Số lượng") +
  theme_minimal()


categorical_vars <- c("Gender","family_history_with_overweight","FAVC",
                      "CAEC","SMOKE","SCC","CALC","MTRANS")

for (var in categorical_vars) {
  ggplot(Obesity, aes_string(x = var, fill = "NObeyesdad_group")) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Tỷ lệ NObeyesdad theo", var),
         x = var,
         y = "Tỷ lệ (%)") +
    theme_minimal() -> p
  print(p)
}

continuous_vars <- c("Age","FCVC","NCP","CH2O","FAF","TUE","BMI")

for (var in continuous_vars) {
  ggplot(Obesity, aes_string(x = "NObeyesdad_group", y = var, fill = "NObeyesdad_group")) +
    geom_boxplot() +
    labs(title = paste("Boxplot", var, "theo NObeyesdad"),
         x = "Tình trạng cơ thể",
         y = var) +
    theme_minimal() +
    theme(legend.position = "none") -> p
  print(p)
}

num_data <- Obesity[, num_vars]

cor_matrix <- cor(num_data, use = "complete.obs")  # loại bỏ NA
round(cor_matrix, 2)  # làm tròn cho dễ đọc

corrplot(cor_matrix, method = "color", 
         type = "upper",           # chỉ vẽ tam giác trên
         addCoef.col = "black",    # thêm hệ số tương quan
         tl.col = "black", tl.srt = 45, # tên biến nghiêng 45 độ
)

## THỐNG KÊ SUY DIỄN

# ANOVA 1 YẾU TỐ: 

# Kiểm tra phân phối chuẩn của BMI trong từng nhóm
by(Obesity$BMI, Obesity$NObeyesdad_group, shapiro.test)
# --- Trực quan hóa Phân phối chuẩn bằng Biểu đồ Q-Q ---

# 1. Tách dữ liệu BMI thành 3 nhóm riêng
# (Đảm bảo NObeyesdad_group không có NA, nếu có dùng:
#  clean_data <- subset(Obesity, !is.na(NObeyesdad_group))
#  normal_bmi <- subset(clean_data, NObeyesdad_group == "Normal")$BMI
# )
normal_bmi <- subset(Obesity, NObeyesdad_group == "Normal")$BMI
overweight_bmi <- subset(Obesity, NObeyesdad_group == "Overweight")$BMI
obesity_bmi <- subset(Obesity, NObeyesdad_group == "Obesity")$BMI

# 2. Thiết lập khung vẽ: 1 hàng, 3 cột
par(mfrow = c(1, 3))

# 3. Vẽ biểu đồ Q-Q cho từng nhóm
qqnorm(normal_bmi, main = "Q-Q Plot: Normal")
qqline(normal_bmi)

qqnorm(overweight_bmi, main = "Q-Q Plot: Overweight")
qqline(overweight_bmi)

qqnorm(obesity_bmi, main = "Q-Q Plot: Obesity")
qqline(obesity_bmi)

# 4. Quay lại thiết lập 1 biểu đồ
par(mfrow = c(1, 1))

leveneTest(BMI ~ NObeyesdad_group, data = Obesity)

anova_model <- aov(BMI ~ NObeyesdad_group, data = Obesity)
summary(anova_model)

qf(p = 0.05, df1 = 2, df2 = 2108, lower.tail = FALSE)


tukey_result <- TukeyHSD(anova_model)
tukey_result

# Vẽ biểu đồ kết quả Tukey
plot(tukey_result)

kruskal.test(BMI ~ NObeyesdad_group, data = Obesity)

library(FSA)
library(dunn.test)

dunn.test(Obesity$BMI, Obesity$NObeyesdad_group, method = "bonferroni")

#HỒI QUY ĐA LỚP:

library(MASS)    # chứa polr() cho ordinal logistic
library(caret)   # confusionMatrix, chia dữ liệu
library(dplyr)


# 70% train, 30% test
set.seed(100)
train_index <- createDataPartition(Obesity$NObeyesdad_group, p = 0.7, list = FALSE)
train_data <- Obesity[train_index, ]
test_data <- Obesity[-train_index, ]

# Xây dựng mô hình đầy đủ với tất cả biến độc lập
# Giả sử các biến số còn lại đã chuẩn hóa/convert sang factor nếu cần
model_full1 <- polr(NObeyesdad_group ~ Gender + Age + family_history_with_overweight +
                     FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC +
                     FAF + TUE + CALC + MTRANS + BMI,
                               data = train_data, Hess = TRUE)
model1 <- step(model_full1)
summary(model1)


model_full2 <- polr(NObeyesdad_group ~ Gender + Age + family_history_with_overweight +
                     FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC +
                     FAF + TUE + CALC + MTRANS,
                   data = train_data, Hess = TRUE)
model2 <- step(model_full2)
summary(model2)

# Dự báo class
pred_class1 <- predict(model1, newdata = test_data)

# Dự báo xác suất
pred_prob1 <- predict(model1, newdata = test_data, type = "probs")

conf_mat <- confusionMatrix(pred_class1, test_data$NObeyesdad_group)
conf_mat


pred_class2 <- predict(model2, newdata = test_data)
test_data_with_pred <- test_data
test_data_with_pred$predicted_value <- pred_class2

# Hiển thị 10 dòng đầu tiên
head(test_data_with_pred, 10)



conf_mat2 <- confusionMatrix(pred_class, test_data$NObeyesdad_group)
conf_mat2
# <<< PHẦN THÊM MỚI: ĐÁNH GIÁ ROC & AUC CHO MODEL 2 >>>

# 1. Tải thư viện pROC (thay thế cho ROCR vì hỗ trợ đa lớp tốt hơn)
# Nếu chưa có, bạn cần chạy: install.packages("pROC")
library(pROC)

# 2. Lấy kết quả xác suất dự đoán từ model2 (bạn đã có)
# (Đảm bảo bạn đã chạy dòng này)
# pred_prob2 <- predict(model2, newdata = test_data, type = "probs")

# 3. Lấy nhãn thực tế từ tập test
true_labels <- test_data$NObeyesdad_group

# --- 4. Tính toán và Vẽ đường ROC cho từng nhóm (One-vs-All) ---

# Thiết lập khung vẽ 1 hàng 3 cột
par(mfrow = c(1, 3))

# --- Đường ROC 1: Nhận diện "Normal" ---
# Tạo biến nhị phân: 1 nếu là "Normal", 0 nếu là khác
is_normal_true <- ifelse(true_labels == "Normal", 1, 0)
# Lấy xác suất dự đoán là "Normal"
prob_normal_pred <- pred_prob2[, "Normal"]

# Tính và vẽ ROC
roc_normal <- roc(is_normal_true, prob_normal_pred)
plot(roc_normal, main = "ROC: Normal vs. Others", col = "blue")

# In ra chỉ số AUC
auc_normal <- auc(roc_normal)
text(0.4, 0.4, paste("AUC =", round(auc_normal, 3)), col = "blue")


# --- Đường ROC 2: Nhận diện "Overweight" ---
is_overweight_true <- ifelse(true_labels == "Overweight", 1, 0)
prob_overweight_pred <- pred_prob2[, "Overweight"]

roc_overweight <- roc(is_overweight_true, prob_overweight_pred)
plot(roc_overweight, main = "ROC: Overweight vs. Others", col = "darkgreen")

auc_overweight <- auc(roc_overweight)
text(0.4, 0.4, paste("AUC =", round(auc_overweight, 3)), col = "darkgreen")


# --- Đường ROC 3: Nhận diện "Obesity" ---
is_obese_true <- ifelse(true_labels == "Obesity", 1, 0)
prob_obese_pred <- pred_prob2[, "Obesity"]

roc_obese <- roc(is_obese_true, prob_obese_pred)
plot(roc_obese, main = "ROC: Obesity vs. Others", col = "red")

auc_obese <- auc(roc_obese)
text(0.4, 0.4, paste("AUC =", round(auc_obese, 3)), col = "red")


# Quay lại thiết lập 1 biểu đồ
par(mfrow = c(1, 1))

# (Tùy chọn) In ra chỉ số AUC tổng hợp (Macro-average)
# multi_roc_result <- multiclass.roc(true_labels, pred_prob2)
# print(multi_roc_result)


# Tạo mô hình tuyến tính "giả lập" chỉ để kiểm tra VIF
# Ta dùng 'Age' làm biến Y tạm thời, vì nó là biến số
vif_check_model <- lm(Age ~ Gender + family_history_with_overweight +
                        FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC +
                        FAF + TUE + CALC + MTRANS,
                      data = train_data)

# Chạy kiểm định VIF
vif(vif_check_model)

# Nếu chưa cài đặt, chạy lệnh: install.packages("brant")
library(brant)

# Thực hiện kiểm định Brant trên model2
# (model2 phải được chạy với Hess=TRUE)
brant_test_result <- brant(model2)

# In kết quả kiểm định
print(brant_test_result)


# --- KIỂM TRA MỐI QUAN HỆ TUYẾN TÍNH GIỮA AGE VÀ LOG-ODDS ---

# 1. Lấy xác suất dự báo cho nhóm 'Normal' từ tập huấn luyện
# (Trong mô hình thứ bậc, P(Normal) chính là xác suất tích lũy đầu tiên)
probs_train <- predict(model2, type = "probs")
prob_normal <- probs_train[, "Normal"]

# 2. Tính Log-odds (Logit) cho việc rơi vào nhóm 'Normal'
# Công thức: log(P / (1-P))
log_odds_normal <- log(prob_normal / (1 - prob_normal))

# 3. Vẽ đồ thị phân tán (Scatter plot)
# Trục hoành: Age (Tuổi)
# Trục tung: Log-odds
par(mar = c(4, 4, 2, 1))
plot(train_data$Age, log_odds_normal,
     main = "Mối quan hệ giữa Age và Log-odds (Normal vs Others)",
     xlab = "Age (Tuổi)",
     ylab = "Log-odds của xác suất thuộc nhóm Normal",
     pch = 19, col = "blue", cex = 0.6) # Trang trí cho đẹp hơn

# Thêm đường xu hướng (Lowess line) để dễ nhìn
lines(lowess(train_data$Age, log_odds_normal), col = "red", lwd = 2)

# --- KIỂM TRA TUYẾN TÍNH CHO NGƯỠNG CÒN LẠI ---

# 1. Lấy xác suất dự báo từ mô hình
probs_train <- predict(model2, type = "probs")

# 2. Tính xác suất tích lũy cho ngưỡng thứ 2: P(Y <= Overweight)
# Nghĩa là: P(Normal) + P(Overweight)
prob_cumulative_overweight <- probs_train[, "Normal"] + probs_train[, "Overweight"]

# 3. Tính Log-odds cho ngưỡng này
# Công thức: log( P(<= Overweight) / P(> Overweight) )
# Tức là: log( P(Normal+Overweight) / P(Obesity) )
log_odds_threshold2 <- log(prob_cumulative_overweight / (1 - prob_cumulative_overweight))

# 4. Vẽ đồ thị
par(mfrow = c(1, 2)) # Chia màn hình làm 2 để so sánh

# Biểu đồ 1: Ngưỡng Normal (Đã vẽ trước đó)
log_odds_normal <- log(probs_train[, "Normal"] / (1 - probs_train[, "Normal"]))
plot(train_data$Age, log_odds_normal,
     main = "Ngưỡng 1: Normal vs. Others",
     xlab = "Age", ylab = "Log-odds (Normal)",
     pch = 19, col = "blue", cex = 0.5)
lines(lowess(train_data$Age, log_odds_normal), col = "red", lwd = 2)

# Biểu đồ 2: Ngưỡng (Normal + Overweight) vs Obesity
plot(train_data$Age, log_odds_threshold2,
     main = "Ngưỡng 2: Non-Obesity vs. Obesity",
     xlab = "Age", ylab = "Log-odds (Normal + Overweight)",
     pch = 19, col = "darkgreen", cex = 0.5)
lines(lowess(train_data$Age, log_odds_threshold2), col = "red", lwd = 2)

# Reset khung vẽ
par(mfrow = c(1, 1))

# 1. Tải thư viện lmtest (chứa hàm dwtest)
# install.packages("lmtest")
library(lmtest)

# 2. Tạo mô hình tuyến tính "giả lập" để kiểm tra
# Ta chuyển biến mục tiêu NObeyesdad_group sang dạng số để chạy lm
# Các biến độc lập được lấy y hệt như trong model2
dw_check_model <- lm(as.numeric(NObeyesdad_group) ~ Age + family_history_with_overweight + 
                       FAVC + FCVC + NCP + CAEC + CH2O + FAF + TUE + CALC + MTRANS, 
                     data = train_data)

# 3. Thực hiện kiểm định Durbin-Watson
dw_result <- dwtest(dw_check_model)

# In kết quả
print(dw_result)

