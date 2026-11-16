std <- data.frame(
  StudyHours = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  Marks = c(35, 40, 45, 50, 55, 62, 68, 72, 80, 85)
)

set.seed(123)
n <- nrow(std)
train_index <- sample(1:n, size = 0.8 * n)
train_data <- std[train_index, ]
test_data <- std[-train_index, ]

model1 <- lm(Marks ~ StudyHours, data = train_data)
pred_marks <- predict(model1, newdata = test_data)

results <- data.frame(
  StudyHours = test_data$StudyHours,
  Actual = test_data$Marks,
  Predicted = pred_marks
)
print(results)

MAE <- mean(abs(test_data$Marks - pred_marks))
MSE <- mean((test_data$Marks - pred_marks)^2)
RMSE <- sqrt(MSE)
R2 <- 1 -
  sum((test_data$Marks - pred_marks)^2) /
  sum((test_data$Marks - mean(test_data$Marks))^2)

cat(sprintf("MAE: %.4f\nMSE: %.4f\nRMSE: %.4f\nR-squared: %.4f\n", MAE, MSE, RMSE, R2))