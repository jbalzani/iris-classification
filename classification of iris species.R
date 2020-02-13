library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- factor(iris$Species, exclude = "setosa")
set.seed(2, sample.kind="Rounding")   
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
test$Species <- factor(test$Species, exclude = "setosa")
train$Species <- factor(train$Species, exclude = "setosa")

#predict species from different features
#define features
x1 <- iris$Sepal.Length
x2 <- iris$Sepal.Width
x3 <- iris$Petal.Length
x4 <- iris$Petal.Width

#generate predictions & evaluate accuracy for each cutoff for each predictor
#sepal length
cutoff_slength <- seq(min(train$Sepal.Length), max(train$Sepal.Length), by = 0.1)
acc_slength <- map_dbl(cutoff_slength, function(x1) {
  yhat_slength <- ifelse(train$Sepal.Length > x1, "virginica", "versicolor") %>%
    factor(levels = levels(y), exclude = "setosa")
  mean(yhat_slength == train$Species)
})
max_acc_slength <- max(acc_slength)
best_cutoff_slength <- cutoff_slength[which.max(acc_slength)]

#sepal width
cutoff_swidth <- seq(min(train$Sepal.Width), max(train$Sepal.Width), by = 0.1)
acc_swidth <- map_dbl(cutoff_swidth, function(x2) {
  yhat_swidth <- ifelse(train$Sepal.Width > x2, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_swidth == train$Species)
})
max_acc_swidth <- max(acc_swidth)
best_cutoff_swidth <- cutoff_swidth[which.max(acc_swidth)]

#petal length
cutoff_plength <- seq(min(train$Petal.Length), max(train$Petal.Length), by = 0.1)
acc_plength <- map_dbl(cutoff_plength, function(x3) {
  yhat_plength <- ifelse(train$Petal.Length > x3, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_plength == train$Species)
})
max_acc_plength <- max(acc_plength)
best_cutoff_plength <- cutoff_plength[which.max(acc_plength)]

#petal width
cutoff_pwidth <- seq(min(train$Petal.Width), max(train$Petal.Width), by = 0.1)
acc_pwidth <- map_dbl(cutoff_pwidth, function(x3) {
  yhat_pwidth <- ifelse(train$Petal.Width > x3, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_pwidth == train$Species)
})
max_acc_pwidth <- max(acc_pwidth)
best_cutoff_pwidth <- cutoff_pwidth[which.max(acc_pwidth)]

#test with test dataset
yhat_plength <- ifelse(test$Petal.Length > best_cutoff_plength, 
                       "virginica", "versicolor") %>%
  factor(levels = levels(y))
mean(yhat_plength == test$Species)

# q10 find best cutoffs from test data
#sepal length
cutoff_slength2 <- seq(min(test$Sepal.Length), max(test$Sepal.Length), by = 0.1)
acc_slength2 <- map_dbl(cutoff_slength2, function(x1) {
  yhat_slength2 <- ifelse(test$Sepal.Length > x1, "virginica", "versicolor") %>%
    factor(levels = levels(y), exclude = "setosa")
  mean(yhat_slength2 == test$Species)
})
max_acc_slength2 <- max(acc_slength2)
best_cutoff_slength2 <- cutoff_slength2[which.max(acc_slength2)]

#sepal width
cutoff_swidth2 <- seq(min(test$Sepal.Width), max(test$Sepal.Width), by = 0.1)
acc_swidth2 <- map_dbl(cutoff_swidth2, function(x1) {
  yhat_swidth2 <- ifelse(test$Sepal.Width > x1, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_swidth2 == test$Species)
})
max_acc_swidth2 <- max(acc_swidth2)
best_cutoff_swidth2 <- cutoff_swidth2[which.max(acc_swidth2)]

#petal length
cutoff_plength2 <- seq(min(test$Petal.Length), max(test$Petal.Length), by = 0.1)
acc_plength2 <- map_dbl(cutoff_plength, function(x3) {
  yhat_plength2 <- ifelse(test$Petal.Length > x3, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_plength2 == test$Species)
})
max_acc_plength2 <- max(acc_plength2)
best_cutoff_plength2 <- cutoff_plength2[which.max(acc_plength2)]

#petal width
cutoff_pwidth2 <- seq(min(test$Petal.Width), max(test$Petal.Width), by = 0.1)
acc_pwidth2 <- map_dbl(cutoff_pwidth2, function(x3) {
  yhat_pwidth2 <- ifelse(test$Petal.Width > x3, "virginica", "versicolor") %>%
    factor(levels = levels(y))
  mean(yhat_pwidth2 == test$Species)
})
max_acc_pwidth2 <- max(acc_pwidth2)
best_cutoff_pwidth2 <- cutoff_pwidth2[which.max(acc_pwidth2)]

#plot
#plot(iris, pch = 21,bg = iris$Species)

#classify species based on both petal length and petal width
#optimize cutoffs for petal length and petal width separately in train dataset
#then apply to test dataset, create rule that predicts virginica if petal length greater
#than cutoff or petal width greater than cutoff
#what is overall accuracy?
yhat_pl_pw <- ifelse(test$Petal.Length > best_cutoff_plength2 | 
                       test$Petal.Width > best_cutoff_pwidth2, "virginica", "versicolor")  %>%
  factor(levels = levels(y))
mean(yhat_pl_pw == test$Species)