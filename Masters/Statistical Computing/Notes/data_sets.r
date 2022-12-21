library(datasets)
data(iris)
View(iris)
str(iris)
dim(iris)
summary(iris)

test_data <- iris
summary(test_data)

test_data$new_column <- 2*test_data$Sepal.Length
View(test_data)

# Trying to remove a column not by index but by name 
new_test_data <- test_data[,-test_data$Sepal.Length]
View(new_test_data)

test_data$Pedal.Width <- as.numeric(test_data$Pedal.Width)
