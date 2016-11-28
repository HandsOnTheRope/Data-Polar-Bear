#PACKAGES


library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(ROCR)
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
library(rattle)

#Read in data
titanicdata <- read.csv("C:/Users/mmigdalovich/Documents/train.csv")
summary(titanicdata)

#split the data using the function
y <- CallieTest(titanicdata,.50, TRUE) 
train<- y$train
test<-y$test

# Create levels
titanicdata$Pclass <- factor(titanicdata$Pclass, levels=c("3","2","1"), ordered=TRUE)
titanicdata$Age[is.na(titanicdata$Age)] <- mean(titanicdata$Age,na.rm=T) # filling in missing ages

# Decision tree
tree <- rpart(Survived ~ Pclass+Sex+Age, data=train, method="class")
printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
summary(tree) # detailed summary of splits

#prune tree
prunetree <-prune(tree,
                  cp= round(tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]),8)

  
fancyRpartPlot(prunetree, uniform=TRUE,
               main="Pruned Tree") # plot the pruned tree 


newdata <-subset(test, select=c(3, 5, 6)) 
My_Prediction<- data.frame(predict(tree, newdata, type="class"))

#misclassification rate
confusionmat <- confusionMatrix(My_Prediction[[1]], test$Survived)  #confusion matrix


#ROC Curve
My_Prediction2<- data.frame(predict(tree, newdata, type="prob"))
pr <- prediction(My_Prediction2[2], test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#MAKE A NEW SCRIPT 
#split function

CallieTest<-function(TD,x,seed=TRUE) {
#titanic data is the given raw data
# x is the % of the data being used in the training of the titanicdata dataset
#seed, sets the same random test & train data set
  
smp_size < - floor (x * nrow(TD))


#Set the seed to make your partition reproductible
if (seed == TRUE) set.seed(123) else 
 print("Did not set seed")
  
train_ind <- sample(seq_len(nrow(TD)), size = smp_size) 
  
  #two new data sets  
train2 <- TD[train_ind, ]
test2 <- TD[-train_ind, ]


result <- list("train"=train2, "test"=test2)
return(result)

}

