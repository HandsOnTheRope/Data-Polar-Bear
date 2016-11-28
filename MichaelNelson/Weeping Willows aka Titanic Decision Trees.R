# Using the Titanic data set to determine if the passenger will survive or not
# Decision Tree
# 11/21/16

# Lib for decision trees
library(rpart)
library(party)
library(rattle)
library(rpart.plot)
library(caret)

#library for ROC curves
library(ROCR)


# Use the train data,  load here make na blank
data <- read.csv("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/train.csv", header = TRUE, na.strings=c(""))


# Check for missing values
    sapply(data,function(x) sum(is.na(x)))

# Age has too many missing values, but is most likely important, we will replace the NA with the average for the data set
    titanic$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# Transform factors Pclass and Sex to factors    
    data$Pclass <- factor(data$Pclass, ordered = TRUE, levels = c("3","2","1"))
    levels(data$Pclass) <- c("Third Class", "Second Class", "First Class")
    data$Sex <- as.factor(data$Sex)

# Split the data into two sets
# 50% of the sample size
    smp_size <- floor(0.5 * nrow(data))

# Set the seed to make your partition reproductible
    set.seed(100)
    trainindex <- sample(seq_len(nrow(data)), size = smp_size)
    
    train <- data[trainindex, ]
    test <- data[-trainindex, ]

# Grow a tree, rpart(formula, data= , method= ,control= ) OR ctree(formula, data)
# We want to predict whether or not the given passenger survives or not
# Below is our standard tree along with four variations.
    fit1 <- rpart(Survived ~ Sex + Age + Pclass, data = train, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))
    
    fit2 <- rpart(Survived ~ Sex + Age + Pclass, data = train, method = "class", control = rpart.control(minsplit = 1, cp = 0.001))
    
    fit3 <- rpart(Survived ~ Sex + Age + Pclass, data = train, method = "class", control = rpart.control(minsplit = 10, cp = 0.01))
    
    fit4 <- rpart(Survived ~ Sex + Age, data = train, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))
    
    fit5 <- rpart(Survived ~ Sex + Age + Pclass + SibSp, data = train, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))

# Examine the results by plotting the trees
    fancyRpartPlot(fit1) 
    fancyRpartPlot(fit2)
    fancyRpartPlot(fit3)
    fancyRpartPlot(fit4)
    fancyRpartPlot(fit5)


# Prune the trees to avoid overfitting
# Want to choose where the xerror from printcp is minimum
    cp.val <- round(fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"])
    pfit1 <- prune(fit1, cp.val)
    
    cp.val <- round(fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])
    pfit2 <- prune(fit2, cp.val)
    
    cp.val <- round(fit3$cptable[which.min(fit3$cptable[,"xerror"]),"CP"])
    pfit3 <- prune(fit3, cp.val)
    
    cp.val <- round(fit4$cptable[which.min(fit4$cptable[,"xerror"]),"CP"])
    pfit4 <- prune(fit4, cp.val)
    
    cp.val <- round(fit5$cptable[which.min(fit5$cptable[,"xerror"]),"CP"])
    pfit5 <- prune(fit5, cp.val)


################# Testing Phase ############################################
# Use the predicted, pruned tree to create a set of Survive/Died predictions for the other dataset --> test.
# In order to apply this testing phase to the original train dataset, you must replace any object below using the "test" data with the "train" data
    
    
# Use predict on the testing data to see how well our models works
    #First, create a new data set that will hold the appropriate test variables.
    #Second, create a dataframe that represents survived (0 or 1) based on the branching process from pfit#.
    newdata1 <-subset(test, select=c(3, 5, 6))
    prediction1 <- data.frame(predict(pfit1, newdata1, type = "class"))
    
    newdata2 <-subset(test, select=c(3, 5, 6))
    prediction2 <- data.frame(predict(pfit2, newdata2, type = "class"))
    
    newdata3 <-subset(test, select=c(3, 5, 6))
    prediction3 <- data.frame(predict(pfit3, newdata3, type = "class"))
    
    newdata4 <-subset(test, select=c(5, 6))
    prediction4 <- data.frame(predict(pfit4, newdata4, type = "class"))
    
    newdata5 <-subset(test, select=c(3, 5, 6, 7))
    prediction5 <- data.frame(predict(pfit5, newdata5, type = "class"))


# Assess the general accuracy of each model. Accuracy is when predicted outcome for survived (0 or 1) matches the actual outcome in the test dataset.
    
    misclassificationError1 <- mean(prediction1 != test$Survived)
    print(paste('Accuracy',1-misclassificationError1))
    
    misclassificationError2 <- mean(prediction2 != test$Survived)
    print(paste('Accuracy',1-misclassificationError2))
    
    misclassificationError3 <- mean(prediction3 != test$Survived)
    print(paste('Accuracy',1-misclassificationError3))
    
    misclassificationError4 <- mean(prediction4 != test$Survived)
    print(paste('Accuracy',1-misclassificationError4))
    
    misclassificationError5 <- mean(prediction5 != test$Survived)
    print(paste('Accuracy',1-misclassificationError5))


# Construct a Confusion matrix manually
# True Negatives - Case correctly predicted to be death
# False Negatives - Case predicted to be death, but actually survived
# False Positives - Case predicted to be survived, but actually death
# True Positives - Case correctly predicted to be survival
      # TN1 <- sum(prediction1 == 0 & test$Survived == 0)
      # FN1 <- sum(prediction1 == 0 & test$Survived == 1)
      # FP1 <- sum(prediction1 == 1 & test$Survived == 0)
      # TP1 <- sum(prediction1 == 1 & test$Survived == 1)


# Construct Confusion Matrices using caret package.
    # confusionMatrix(prediction data, reference data)
    confusionmat1 <- confusionMatrix(prediction1[[1]], test$Survived)
    confusionmat1
    
    confusionmat2 <- confusionMatrix(prediction2[[1]], test$Survived)
    confusionmat2
    
    confusionmat3 <- confusionMatrix(prediction3[[1]], test$Survived)
    confusionmat3
    
    confusionmat4 <- confusionMatrix(prediction4[[1]], test$Survived)
    confusionmat4
    
    confusionmat5 <- confusionMatrix(prediction5[[1]], test$Survived)
    confusionmat5


    
# ROC Curve & Area Under the Curve (AUC) for the models

    # Predroc# is the continuous prediction of the classification, the labels are the binary truth for each variable
    predroc1 <- data.frame(predict(pfit1, newdata1, type = "prob"))
    pr1 <- prediction(predroc1[2], test$Survived)
    prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
    plot(prf1)
    
    auc1 <- performance(pr1, measure = "auc")
    auc1 <- auc1@y.values[[1]]
    auc1
    
    
    predroc2 <- data.frame(predict(pfit2, newdata2, type = "prob"))
    pr2 <- prediction(predroc2[2], test$Survived)
    prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
    plot(prf2)
    
    auc2 <- performance(pr2, measure = "auc")
    auc2 <- auc2@y.values[[1]]
    auc2
    
    
    predroc3 <- data.frame(predict(pfit3, newdata3, type = "prob"))
    pr3 <- prediction(predroc3[2], test$Survived)
    prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
    plot(prf3)
    
    auc3 <- performance(pr3, measure = "auc")
    auc3 <- auc3@y.values[[1]]
    auc3
    
    
    predroc4 <- data.frame(predict(pfit4, newdata4, type = "prob"))
    pr4 <- prediction(predroc4[2], test$Survived)
    prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
    plot(prf4)
    
    auc4 <- performance(pr4, measure = "auc")
    auc4 <- auc4@y.values[[1]]
    auc4
    
    
    predroc5 <- data.frame(predict(pfit5, newdata5, type = "prob"))
    pr5 <- prediction(predroc5[2], test$Survived)
    prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
    plot(prf5)
    
    auc5 <- performance(pr5, measure = "auc")
    auc5 <- auc5@y.values[[1]]
    auc5
    
    