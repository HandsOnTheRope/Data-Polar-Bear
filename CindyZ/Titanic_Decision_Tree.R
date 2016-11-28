#R-Training
#Decision Tree Modelling 
#Titanic Dataset
#11/21/16

#Sections:
# A.Split Function
# 1.Loading the data
# 2.Cleaning the data 
# 3.Creating new variables
# 4.Splitting the data
# 5.Summary Statistics
# 6.Decision Trees


############# SPLIT FUNCTION #############   

split_function <- function(df2,x,seed=TRUE) {
  #df is the raw data frame to augment
  #x is the percent of data in training df
  #seed is a boolean, if want it to use the seed
  
  #x% of the sample size
  smp_size <- floor(x * nrow(df2))
  
  #Set the seed to make your partition reproductible
  if (seed == TRUE) set.seed(123) else 
    print("Did not set seed")
  
  train_ind <- sample(seq_len(nrow(df2)), size = smp_size)
  
  #Create 2 data frames
  train2 <- df2[train_ind, ]
  test2 <- df2[-train_ind, ]
  
  #Create list of 2 df to return
  result <- list("Train"=train2,"Test"=test2)
  return(result)
  
}  

############# LOADING THE DATA ############# 

#Install extra packages if don't have them
  #install.packages("broom") #for better output of regressions
  #install.packages("ggplot2")
  #install.packages("tree")
  #install.packages("rpart")
  #install.packages("party")
  #install.packages("rattle")
  #install.packages("caret")
  #install.packages("rpart.plot")
  #install.packages("RColorBrewer")
  #install.packages("e1071")
  #install.packages("pROC")
  #install.packages("ROCR")


#Load packages
library(party)
library(rpart)
library(rattle)
library(rpart)  
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(lattice)
library(e1071)
library(pROC)
library(ROCR)


#Set working directory
  setwd("~/IRS OCA RAAS/Trainings/R_Studio/Titanic") 

#Imports csv file
  all.data <-
    read.csv(
      "C:/Users/cizheng/Documents/IRS OCA RAAS/Trainings/R_Studio/Titanic/train.csv",
      header = TRUE
    )


#Set libraries
  library(ggplot2)
  library(tree)
  
  
############# CLEANING THE DATA ############# 
  
#Adressing missing values. 
  #Variables missing values: Age, Cabin, and Embarked 
  
  #Age: 92 NA's. Because this is a potentially important variable of interest, it doesn't make sense to drop these.
    #Instead, will fill in with the mean of the age.
    
    #Calculate the mean age
      all.data$age_mean <- mean(all.data$Age,na.rm = TRUE)
      
    #Create new varialbe with non missing values
      all.data$age_clean <- ifelse(is.na(all.data$Age), all.data$age_mean, all.data$Age)
    
  #Cabin: 91 NA's. Not as important of a variable. Class captures many of the same characteristics. No way to fill in.
    
  #Embarked: 2 NA's. Could potentially just drop those 2. Will see if need to use it later. 

      
#Breaking up names into first, last
  #Will do this in excel if have time ********

#Converting to correct variable type
  #Numeric to factor
      #Survived
        all.data$Survived_factor <- factor(all.data$Survived, levels = c(1,0), labels = c("Survived", "Died"))
        
      #Pclass
        all.data$Pclass_factor <- factor(all.data$Pclass, levels = c(3,2,1), labels = c("Lower Class", "Middle Class", "Upper Class"))
      
      
############# CREATING NEW VARIABLES ############# 
        
        #Family size variable is the sum of all siblings, spouses, parents, and children on board
        all.data$family_sz <- all.data$SibSp + all.data$Parch
        
        #Family flag to indicate wheather have a family
        all.data$family_fl <- factor(ifelse(all.data$family_sz != 0, 1, 0),
                               levels = c(1,0),
                               labels = c("Has Family on Ship", "No Family on Ship"))      
        
        #Child flag (if age < 18)
        all.data$child <- factor(ifelse(all.data$age_clean < 18, 1, 0),
                           levels = c(1,0),
                           labels = c("Child", "Not Child"))
        
        #Adult flag (if age 18-64)
        all.data$adult <- factor(ifelse(all.data$age_clean > 17 & all.data$age_clean <65, 1, 0),
                           levels = c(1,0),
                           labels = c("Adult", "Not Adult"))
        
        #Senior flag (if age > 64)
        all.data$senior <- factor(ifelse(all.data$age_clean > 64, 1, 0),
                            levels = c(1,0),
                            labels = c("Senior", "Not Senior"))

############# SPLITTING THE DATA ############# 
        
#Invoking the function to randomly split the data frame into train and test
  y <- split_function(all.data, .5, TRUE)
  
  #Access both data frames
  #y$Train
  #y$Test  
  
  
#Could also use k-folds to cross validate split
        
#Set the data frame to use
df <- y$Train
      
############# SUMMARY STATISTICS ############# 

#General dataset summary stats
  str(df)
  names(df)
  summary(df)
  class(df)
 # cor(all.data[c("FICO.Score","Interest.Rate.Clean", "Loan.Length.Clean")])
  
#Scatter plot of age and survival
  plot(Survived_factor~age_clean, data=df)
  abline(lm(Survived_factor~age_clean)) 

#Scatter plot of age and sex
  plot(Sex~age_clean, data=df)
  abline(lm(Survived_factor~age_clean)) 

#Histogram of age
  hist(df$age_clean,
       breaks = 30)
  
# Simple Bar Plot of number of survivors
  counts <- table(df$Survived)
  barplot(counts, main="Survivors", 
          xlab="Survived")
  
  
# *****Can continue to show more charts *******
  


############# DECISION TREES ############# 

  #Party package resources: https://www.tutorialspoint.com/r/r_decision_tree.htm
  #General resources: http://www.edureka.co/blog/implementation-of-decision-tree/
    #https://rpubs.com/ryankelly/dtrees
    #https://rpubs.com/aaronsc32/predict-extramaterial-affairs-with-decision-trees-and-r
  
#Creat tree using rpart
  output.tree <- rpart(
    Survived_factor ~ age_clean + Pclass_factor + Sex + family_sz, 
    data = df, method="class")

# Plot the tree
  fancyRpartPlot((output.tree))

#Cross validation using complexity parameter
  printcp(output.tree)
  
#Plotcp: rovides a graphical representation to the cross validated error summary. 
  #The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached.
  plotcp(output.tree)

#Summary of splits
  summary(output.tree)
  

#Prune tree to avoid overfitting
  ptree<- prune(output.tree,
                cp= round(output.tree$cptable[which.min(output.tree$cptable[,"xerror"]),"CP"],8))
  #NOTE: FOR SOME REASON, IT PRUNES OFF ALL BUT ONE BRANCH - SEX. NOT AFFECTED BY WHICH TRAIN SUBSET IS SELECTED FROM SET SEED.
  
#Plot new pruned tree
  #plot(ptree)
  fancyRpartPlot(ptree, uniform=TRUE,
                 main="Pruned Classification Tree")
  
 
#Using pruned tree to make predictions on the test set
  #labeling test dataset
    test_df <- y$Test 
  tree.pred <- predict(ptree, test, type = "class")
  
  #Prints out table of results
  table(tree.pred, test$Survived_factor)
  
# Use predict on the testing data to see how well our model works
  newdata <-subset(test_df, select=c(5,14,16,17))
  prediction <- data.frame(predict(ptree, newdata, type = "class"))
  
#Confusion Matrix

  confusionmat <- confusionMatrix(prediction[[1]], test_df$Survived_factor)
  confusionmat
  
#ROC Curve
  
  # calculating the values for ROC curve
  #pred <- prediction(target_pred, target_class)
  predic_roc <- data.frame(predict(ptree, newdata, type = "prob"))
  pr <- prediction(predic_roc[1], test_df$Survived_factor)
  perf <- performance(pr,"tpr","fpr")
  plot(perf)
  
  # changing params for the ROC plot - width, etc
  par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  # plotting the ROC curve
  plot(perf,col="black",lty=3, lwd=3)
  # calculating AUC
  auc <- performance(pr,"auc")
  # now converting S4 class to vector
  auc <- unlist(slot(auc, "y.values"))
  #Printing out AUC area
  auc
######################################
