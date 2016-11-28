#import data
library(readr)
mydata <- read_csv("~/train.csv")

#import rpart function
library(rpart)

#make family size variable  
mydata$famsize <- mydata$SibSp + mydata$Parch + 1

#split data into testing and training dataset 
dt = sort(sample(nrow(mydata), nrow(mydata)*.5))
train <- mydata[dt,]
test <- mydata[-dt,]

#make decision tree
tree2 <- rpart(Survived ~  Sex + Age + Pclass + famsize, data=train, method="class")
summary(tree2)
printcp(tree2)

#display decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree)

#trim tree
cp.val <- round(tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"], 8)
ptree <- prune(tree2, cp.val)
plot(ptree)
fancyRpartPlot(ptree)

#predict test data 
newdata1 <-subset(test, select=c(3, 5, 6, 14))
prediction1 <- data.frame(predict(ptree, newdata1, type = "class"))

#compare predictions to actual test data
confusionmat1 <- confusionMatrix(prediction1[[1]], test$Survived)
print(confusionmat1)

#make ROC curve
install.packages("ROCR")
library(ROCR)
prediction2 <- data.frame(predict(ptree, newdata1, type = "prob"))
pred <- prediction(prediction2[2], test$Survived)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf)

#area under curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc 
