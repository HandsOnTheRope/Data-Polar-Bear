# Using the Titanic data set to determine if the passenger will survive or not
# Decision Tree
# 11/21/16

# Library for decision trees
library(rpart)
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# library(e1071) 
# Library for k-fold cross validation function
library(caret)
# Library for ROC curves
library(ROCR)

# Splitting data function
source("C:/Users/jdumiak/Documents/Titanic/dataSplit.R")

# Use the train data,  load here make na blank
titanic <- read.csv("C:/Users/jdumiak/Documents/Titanic/train.csv", header = TRUE, na.strings=c(""))
# 1 is survived, 0 is died 
# For class, 1 = 1st class, 2 = 2nd class, 3 = third class

# Check for missing values
sapply(titanic,function(x) sum(is.na(x)))
# Cabin has way too many missing values, drop this variable immeadiately
# Also Pasenger ID since it is an index and ticket
# Check for unique values
sapply(training.data.raw, function(x) length(unique(x)))

# Age has too many missing values, but is most likely important, we will replace the NA with the average for the data set
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)

# Factor data
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c("3","2","1"))
# Rename, 1st class is better than third class
levels(titanic$Pclass) <- c("Third Class", "Second Class", "First Class")
# Not sure if we need to factor age 
# titanic$Age <- as.factor(titanic$Age) 
titanic$Sex <- as.factor(titanic$Sex)

# Commented out section here was put into function dataSplit.R
# # Split the data into two sets
# # 50% of the sample size
# smp_size <- floor(0.5 * nrow(titanic))
# 
# # Set the seed to make your partition reproductible
# set.seed(123)
# trainindex <- sample(seq_len(nrow(titanic)), size = smp_size)
# 
# train <- titanic[trainindex, ]
# test <- titanic[-trainindex, ]

# Function to split data, dataSplit
outputList <- dataSplit(titanic, 0.5, 123)
titanic <- data.frame(outputList[1])
train <- data.frame(outputList[2])
test <- data.frame(outputList[3])

# Grow a tree, rpart(formula, data= , method= ,control= ) OR ctree(formula, data)
# We want to predict whether or not the given passenger survives or not
fit <- rpart(Survived ~ Sex + Age + Pclass, data = train, method = "class")
# Add interactions between Sex and Class and Age and Class??, trees cannot handle interaction terms so no

# Examine the results 
# Validation of the tree
printcp(fit) # display the results, this function provides the optimal prunings based on the cp value
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# Prune the tree to avoid overfitting
# Want to choose where the xerror from printcp is minimum
cp.val <- round(fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"], 8)
pfit <- prune(fit, cp.val)

# Plot the tree
fancyRpartPlot(pfit) 

# Use predict on the testing data to see how well our model works
newdata <-subset(test, select=c(3, 5, 6))
pred <- data.frame(predict(pfit, newdata, type = "class"))

# Use 0.5 prediction boundary with type="prob"
# prediction <- ifelse(prediction > 0.5,1,0)

misclassificationError <- mean(pred != test$Survived)
print(paste('Accuracy',1-misclassificationError))

# Plots to visualize performance & evaluation
# Confusion matrix
# True Negatives - Case correctly predicted to be death
# False Negatives - Case predicted to be death, but actually survived
# False Positives - Case predicted to be survived, but actually death
# True Positives - Case correctly predicted to be survival
TN <- sum(pred == 0 & test$Survived == 0)
FN <- sum(pred == 0 & test$Survived == 1)
FP <- sum(pred == 1 & test$Survived == 0)
TP <- sum(pred == 1 & test$Survived == 1)
confusionmat <- confusionMatrix(pred[[1]], test$Survived)

# ROC Curve + AUC
# Predictions are your continuous predictions of the classification, the labels are the binary truth for each variable
predroc <- data.frame(predict(pfit, newdata, type = "prob"))
pr <- prediction(predroc[2], test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
