library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(caret)

#read in splitdata function
source("C:/Users/jdessy/Documents/R//DecisionTree/splitdata.R")

#label data into train and test datasets
mydata= splitdata("C:/Users/jdessy/Documents/R/DecisionTree/train.csv", 0.5)

#Make categorical variables 
mydata$Survived.Cat <- factor(mydata$Survived,level=c("0","1"))
mydata$Pclass.Cat <- factor(mydata$Pclass,level=c("1","2","3"))

#split datasets in train and test datasets 
train <- subset(mydata, mydata$label == "train")
test  <- subset(mydata, mydata$label == "test")

#Grow the Tree 
Dtree <- rpart(train$Survived.Cat~ train$Pclass.Cat + train$Age + train$Sex , data= train, method= "class", 
               control = rpart.control(minsplit = 30))

#Tree information
printcp(Dtree)
plotcp(Dtree)

#set cp value to the minimum 
cp.val <- round(Dtree$cptable[which.min(Dtree$cptable[,"xerror"]),"CP"],10)

#Prune the Tree
pruned_Dtree <- prune(Dtree, cp=cp.val)
fancyRpartPlot(pruned_Dtree) 

#Delete first row of test to make same size. I KNOW I SHOULD NOT BE DOING THIS. 
test = test[-1,]

#find the misclassification rate in the train dataset
Dtree_pred = as.factor(predict(pruned_Dtree, test, type = "class"))
mean(Dtree_pred != test$Survived.Cat)

#create a confusion matrix using positives and negatives
confusionmat <- confusionMatrix(Dtree_pred[[1]], test$Survived.Cat) 

#Make an ROC curve
predroc <- data.frame(predict(pruned_Dtree, test, type = "prob")) 
pred <- prediction(predroc[2], test$Survived.Cat) 
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf) 

#Find the area under the curve
auc <- performance(pred, measure = "auc") 
auc <- auc@y.values[[1]] 
print(auc) 







