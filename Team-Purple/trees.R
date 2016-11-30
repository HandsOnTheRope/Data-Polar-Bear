
### MEMORY #################################################################################################################

#Clear Memory
rm(list = ls()) 
gc()


### LOAD DATA ##############################################################################################################

#Set Directory 
setwd("C:/Users/sampahwa/Documents/R Scripts")

#Read in Data 
titanic <- read.csv("train.csv")
names(titanic)
attach(titanic)


### SPLIT DATA #############################################################################################################

#Make categorical variables 
titanic$Survived.Cat <- factor(titanic$Survived,level=c("0","1"))
titanic$Pclass.Cat <- factor(titanic$Pclass,level=c("1","2","3"))

#50% of the sample size
size <- floor(0.5 * nrow(titanic))

#Set the seed to make your partition reproductible
set.seed(445)
half <- sample(seq_len(nrow(titanic)), size = size)

train <- titanic[half, ]
test <- titanic[-half, ]


### BUILD TREE #############################################################################################################

library(rpart)

redwood <- rpart(Survived ~ Pclass + Age + Sex, data=train, method="class", control = rpart.control(minsplit = 30), cp=0.0001)

printcp(redwood)

plot(redwood, uniform = TRUE, main="Redwood Tree")
text(redwood, use.n=TRUE, all=TRUE, cex=.8)


### PRUNE TREE #############################################################################################################

maple <- prune(redwood, cp =redwood$cptable[which.min(redwood$cptable[,"xerror"]),"CP"])
               
plot(maple, uniform = TRUE, main="Maple Tree")
text(maple, use.n=TRUE, all=TRUE, cex=.8)

### PARTY ##################################################################################################################

library(party)

ct <- ctree(redwood, data = train)
plot(ct, main="Conditional Inference Tree")

#Table of prediction errors
table(predict(ct), train$Survived)

# Estimated class probabilities
tr.pred = predict(ct, newdata=train, type="prob")


### BUILD TREE #############################################################################################################

library(rpart)

sycamore <- rpart(Survived ~ Pclass + Age + Sex, data=test, method="class")

printcp(sycamore)

plot(sycamore, uniform = TRUE, main="Sycamore Tree")
text(sycamore, use.n=TRUE, all=TRUE, cex=.8)


### PRUNE TREE #############################################################################################################

palm <- prune(sycamore, cp =sycamore$cptable[which.min(sycamore$cptable[,"xerror"]),"CP"])

plot(palm, uniform = TRUE, main="Palm Tree")
text(palm, use.n=TRUE, all=TRUE, cex=.8)


### PARTY ##################################################################################################################

library(party)

ct <- ctree(palm, data = test)
plot(ct, main="Conditional Inference Tree")

#Table of prediction errors
table(predict(ct), test$Survived)

# Estimated class probabilities
tr.pred = predict(ct, newdata=test, type="prob")

