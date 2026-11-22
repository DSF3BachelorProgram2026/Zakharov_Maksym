suppressMessages(library(tidymodels))
fit <- readRDS("artifacts_lab7/iris_best_workflow.rds")
new_observation <- data.frame(
  Sepal.Length = 6.3, Sepal.Width = 3.0,
  Petal.Length = 5.1, Petal.Width = 1.8)
print(predict(fit, new_data = new_observation, type = "prob"))
print(predict(fit, new_data = new_observation, type = "class"))
