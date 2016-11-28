# install.packages('tree')
library(tree)
# install.packages('party')
library(party)
#install.packages('rattle')
library(rattle)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('rpart')
library(rpart)
#install.packages('caret')
library(caret)
#install.packages('lattice')
library(lattice)
#install.packages('RColorBrewer')
library(RColorBrewer)

titanicData <- read.csv(choose.files(), stringsAsFactors = FALSE)
summary(titanicData)
dim(titanicData)
str(titanicData)
titanicData$Survived <- factor(titanicData$Survived)
titanicData$Sex <- factor(titanicData$Sex)
titanicData$Embarked <- factor(titanicData$Embarked)
titanicData$Age <- as.numeric(titanicData$Age)

# Sample Indexes
indexes = sample(1:nrow(titanicData), size = .5*nrow(titanicData))

# Split Data
test = titanicData[indexes,]
dim(test) #445 x 12
train = titanicData[-indexes,]
dim (train) # 446 x 12

# Seed
set.seed(2)

# fill in misssing Age data
titanicData$Age[is.na(titanicdata$Age)] <- mean(titanicData$Age,na.rm=T) 
tree_model <- rpart(Survived ~ train$Pclass + train$Sex + train$Age + train$SibSp + train$Parch, data = train, method = "class")

# display results
printcp(tree_model) 

# plot cross-validation visualization
plotcp(tree_model)

# summary of splits
summary(tree_model)

# prune tree
tree_model_prune <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])

# plot pruned tree 
fancyRpartPlot(tree_model_prune, uniform=TRUE, main="Decision Tree")