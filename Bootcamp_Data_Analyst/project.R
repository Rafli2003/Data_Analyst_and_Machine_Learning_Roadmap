# Load library yang diperlukan
library(caret)
library(ggplot2)

# Load dan siapkan data
library(readr)
df1 <- read_csv("D:/student_monnitoring_data.csv")
View(df1)

summary(df1)

df1 <- df1[, !(names(df1) %in% c("Student.ID", "Date", "Class.Time"))]

df1$`Attendance Status` <- as.numeric(factor(df1$`Attendance Status`))
df1$`Risk Level`<- as.numeric(factor(df1$`Risk Level`))

# Membagi data menjadi training dan testing
set.seed(123)
training_index <- createDataPartition(df1$`Risk Level`, p = 0.8, list = F)
train_data <- df1[training_index, ]
test_data <- df1[-training_index, ]

# 1. Membuat ketiga model dan Interpretasi

# Model 1 : Regresi Linear Sederhana
model_sederhana <- lm(`Risk Level` ~ `Stress Level (GSR)`, data = train_data)
summary(model_sederhana)

# Model 2 : Regresi Linear Berganda
model_berganda <- lm(`Risk Level` ~ `Stress Level (GSR)` + `Sleep Hours` + `Anxiety Level` + `Mood Score`, data = train_data)
summary(model_berganda)

# Model 3 : Regresi Polinomial
model_poly <- lm(`Risk Level` ~ `Stress Level (GSR)` + I(`Stress Level (GSR)`^2), data = train_data)
summary(model_poly)

# 2. Fungsi untuk Menghitung R* dan RMSE
calculate_metrics <- function(model, test_data){
  predictions <- predict(model, newdata = test_data)
  r2 <- 1 - sum((test_data$Risk.Level - predictions)^2) / sum((test_data$Risk.Level - mean(test_data$Risk.Level))^2)
  rmse <- sqrt(mean((test_data$Risk.Level - predictions)^2))
  return(c(R_squared = r2, RMSE = rmse))
}

# 3. Menghitung Metrics untuk setiap Model
metrics_sederhana <- calculate_metrics(model_sederhana, test_data)
metrics_berganda <- calculate_metrics(model_berganda, test_data)
metrics_poly <- calculate_metrics(model_poly, test_data)

comparison_table <- rbind(
  "Model Sederhana" = metrics_sederhana,
  "Model Berganda" = metrics_berganda,
  "Model Polinomial" = metrics_poly
)

print(round(comparison_table, 4))


# 4. Visualisasi Perbandingan Metrics
par(mfrow = c(1,2))
barplot(comparison_table[,1], main = "Perbandingan R-squared", col = c("skyblue", "lightgreen", "pink"))
barplot(comparison_table[,2], main = "Perbandingan RMSE", col = c("skyblue", "lightgreen", "pink"))

grid()


# 5. Menentukan Model Terbaik
best_r2 <- which.max(comparison_table[,1])
best_rmse <- which.min(comparison_table[,2])

cat("\nModel terbaik berdasarkan R-squared:", rownames(comparison_table)[best_r2])
cat("\nModel terbaik berdasarkan RMSE:", rownames(comparison_table)[best_rmse])

# 6. Visualisasi Prediksi Model Terbaik
best_model <- if(best_rmse == best_r2) {
  list(model = get(paste0("model_", tolower(strsplit(rownames(comparison_table)[best_rmse], " ")[[1]][2]))),
       name = rownames(comparison_table)[best_rmse])
} else {
  cat("\n\nPerhatian: Model terbaik berbeda berdasarkan R-squared dan RMSE.")
  list(model = get(paste0("model_", tolower(strsplit(rownames(comparison_table)[best_rmse], " ")[[1]][2]))),
       name = rownames(comparison_table)[best_rmse])
}

predictions <- predict(best_model$model, newdata = test_data)
plot(test_data$Risk.Level, predictions, main = paste("Actual vs Predicted -", best_model$name), xlab = "Actual Values", ylab = "Predicted Values")
abline(0, 1, col = "red")
