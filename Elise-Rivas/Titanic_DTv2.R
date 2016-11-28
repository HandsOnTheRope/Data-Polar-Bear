# Import libraries
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ROCR)
library(caret)
library(pROC)
#-------------------------------------------------
# Load Data
setwd("C:/Users/elrivas/Documents/Trainings/R Training/Titanic")
total_data <- read.csv("C:/Users/elrivas/Documents/Trainings/R Training/Titanic/train.csv")
# Set missing age values to mean
total_data$Age[is.na(total_data$Age)] <- mean(total_data$Age, na.rm=TRUE)
# New fanm size variable
total_data$fam_size <- (total_data$SibSp+total_data$Parch)+1
# Factor variables
total_data$Survived <- as.factor(total_data$Survived)
total_data$Embarked <- as.factor(total_data$Embarked)
total_data$Sex <- as.factor(total_data$Sex)
total_data$Pclass <- factor(total_data$Pclass, levels=c(3,2,1), order = TRUE)
#-------------------------------------------------
# Data splitting function
split_function <- function(raw, prop.split){
  # randomize
  raw <- raw[sample(1:nrow(raw)),]
  # new variable, make default "train" for now
  raw$test_or_train <- "train"
  raw$test_or_train[sample(1:nrow(raw), size=((prop.split)*nrow(raw)))] <- "test"
  return(raw)
}
# Run function
new_data <- split_function(total_data, .5)
# Separate into two
train <- subset(new_data, test_or_train=="train")
test <- subset(new_data, test_or_train=="test")
#-------------------------------------------------
# Create trees with variables that might be good indicators of survival
fit1 <- rpart(Survived ~ Pclass + Sex + Fare + Age + fam_size, data=train)
fit2 <- rpart(Survived ~ Pclass + Sex + Fare + Age + SibSp + Parch + fam_size, data=train)
fit3 <- rpart(Survived ~ Pclass + Sex + Age + fam_size, data=train)
fit_all <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + fam_size, data=train)
# Prune, use best cp
p_fit1 <- prune(fit1, cp= fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"])
p_fit2 <- prune(fit2, cp= fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])
p_fit3 <- prune(fit3, cp= fit3$cptable[which.min(fit3$cptable[,"xerror"]),"CP"])
p_fit_all <- prune(fit_all, cp= fit_all$cptable[which.min(fit_all$cptable[,"xerror"]),"CP"])
#--------------------------------------------------
# Predict with unpruned
pred1 <- predict(fit1, test, type="class")
pred2 <- predict(fit2, test, type="class")
pred3 <- predict(fit3, test, type="class")
pred_all <- predict(fit_all, test, type="class")
# Predict with pruned
p_pred1 <- predict(p_fit1, test, type="class")
p_pred2 <- predict(p_fit2, test, type="class")
p_pred3 <- predict(p_fit3, test, type="class")
p_pred_all <- predict(p_fit_all, test, type="class")
#--------------------------------------------------
# Confusion matrices, calculating accuracy rates for non-pruned models
ar_pred1 <- confusionMatrix(data=pred1, reference = test$Survived)$overall[1]
ar_pred2 <- confusionMatrix(data=pred2, reference = test$Survived)$overall[1]
ar_pred3 <- confusionMatrix(data=pred3, reference = test$Survived)$overall[1]
ar_pred_all <- confusionMatrix(data=pred_all, reference = test$Survived)$overall[1]
# Confusion matrices, calculating accuracy rates for pruned models
ar_p_pred1 <- confusionMatrix(data=p_pred1, reference = test$Survived)$overall[1]
ar_p_pred2 <- confusionMatrix(data=p_pred2, reference = test$Survived)$overall[1]
ar_p_pred3 <- confusionMatrix(data=p_pred3, reference = test$Survived)$overall[1]
ar_p_pred_all <- confusionMatrix(data=p_pred_all, reference = test$Survived)$overall[1]
#--------------------------------------------------
# return model with best accuracy rate
ar_list <- c(ar_pred1, ar_pred2, ar_pred3, ar_pred_all,
             ar_p_pred1, ar_p_pred2, ar_p_pred3, ar_p_pred_all)
which(ar_list==max(ar_list))
# ^ Returns different indexes each time program is run because data is randomized at beginning
#--------------------------------------------------
