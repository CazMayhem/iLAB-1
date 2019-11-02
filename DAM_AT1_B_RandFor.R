#### Exercise 3: Random Forest ####

#### Introduction ####

rm(list=ls())
library(rpart)
library(mlbench)
library(randomForest)

setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")
repRead<- read.csv("repurchase_training.csv")

# reclassify/merge some of the lower value car_model groupings
recastData <- repRead %>% select(-ID) %>%
  mutate(car_model = as.factor(ifelse(as.character(car_model) == "model_9" | 
                                        as.character(car_model) == "model_10" | as.character(car_model) == "model_12" | 
                                        as.character(car_model) == "model_14" | as.character(car_model) >= "model_16" & 
                                        as.character(car_model) <= "model_19", "model_0", as.character(car_model))))

# remove some dimensions from dataset for modelling
repurchaseData <- recastData %>% subset(select = -c(age_band,gender,car_model,car_segment,age_of_vehicle_years,non_sched_serv_warr,total_paid_services))

# Find name of predicted variable
names(repurchaseData)

# Get index of predicted variable
class_col_num <- grep("Target",names(repurchaseData))

# Create training and test sets
## 80% of the sample size, use floor to round down to nearest integer
trainset_size <- floor(0.80 * nrow(repurchaseData))

# First step is to set a random seed to ensurre we get the same result each time
set.seed(42) 

# Get indices of observations to be assigned to training set...
# This is via randomly picking observations using the sample function

trainset_indices <- sample(seq_len(nrow(repurchaseData)), size = trainset_size)

# Assign observations to training and testing sets

trainset <- repurchaseData[trainset_indices, ]
testset <- repurchaseData[-trainset_indices, ]

trainset$Target <- as.factor(trainset$Target)
testset$Target  <- as.factor(testset$Target)

# Rowcounts to check
nrow(trainset)
nrow(testset)
nrow(repurchaseData)

#### Decision tree for comparison ####

# Build model
rpart_model <- rpart(Target ~.,data = trainset, method="class")

# Plot tree
prp(rpart_model)

# Get predictions
rpart_predict <- predict(rpart_model,testset[,-class_col_num],type="class")
mean(rpart_predict==testset$Target)

# Confusion matrix
table(rpart_predict,testset$Target)

#Estimate average accuracy for an rpart model on this dataset
multiple_runs_rpart <- function(df,class_variable_name,train_fraction,nruns){
  
  #Purpose:
  #Builds rpart model for nrun data partitions
  
  #Return value:
  #Vector containing nrun accuracies
  
  #Arguments:
  #df: variable containing dataframe
  #class_variable_name: class name as a quoted string. e.g. "Class"
  #train_fraction: fraction of data to be assigned to training set (0<train_fraction<1)
  #nruns: number of data partitions
  
  # Find column index of class variable
  class_col_num <- grep(class_variable_name,names(df))
  # Initialize accuracy vector
  accuracies <- rep(NA,nruns)
  # Set seed (can be any integer)
  set.seed(42)
  for (i in 1:nruns){
    # Partition data
    trainset_size <- floor(train_fraction * nrow(df))
    trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
    trainset <- df[trainset_indices, ]
    testset <- df[-trainset_indices, ]
    # Bbuild model 
    # Paste builds formula string and as.formula interprets it as an R formula
    rpart_model <- rpart(as.formula(paste(class_variable_name,"~.")),data = trainset, method="class")
    # Predict on test data
    rpart_predict <- predict(rpart_model,testset[,-class_col_num],type="class")
    # Accuracy
    accuracies[i] <- mean(rpart_predict==testset[[class_variable_name]])
  }
  return(accuracies)
}

# Calculate average accuracy and std dev over 30 random partitions
accuracy_results <- multiple_runs_rpart(trainset,"Target",0.8,30)
mean(accuracy_results)
sd(accuracy_results)
accuracy_results

#### Random Forest ####

# Build random forest model
Repurchase.rf <- randomForest(Target ~.,data = trainset, 
                              importance=TRUE, xtest=testset[,-class_col_num],ntree=1000)

# What other things could we set? 
# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest

# Model summary
# Not super useful for model analysis, but 
summary(Repurchase.rf) 

# Objects returned from the model 
names(Repurchase.rf)

# Predictions for test set
test_predictions_rf <- data.frame(testset,Repurchase.rf$test$predicted)

# Accuracy for test set
mean(Repurchase.rf$test$predicted==testset$Target)

# Confusion matrix
table(Repurchase.rf$test$predicted,testset$Target)

# Quantitative measure of variable importance
importance(Repurchase.rf)

# Sorted plot of importance
varImpPlot(Repurchase.rf)

#Homework: estimate average accuracy for a randomForest model on this dataset. You will need to write a function to do this.
multiple_runs_rf <- function(df,class_variable_name,train_fraction,nruns,nTree){
  
  #Purpose:
  #Builds rpart model for nrun data partitions
  
  #Return value:
  #Vector containing nrun accuracies
  
  #Arguments:
  #df: variable containing dataframe
  #class_variable_name: class name as a quoted string. e.g. "Class"
  #train_fraction: fraction of data to be assigned to training set (0<train_fraction<1)
  #nruns: number of data partitions
  
  # Find column index of class variable
  class_col_num <- grep(class_variable_name,names(df))
  # Initialize accuracy vector
  accuracies <- rep(NA,nruns)
  # Set seed (can be any integer)
  set.seed(42)
  for (i in 1:nruns){
    # Partition data
    trainset_size <- floor(train_fraction * nrow(df))
    trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
    trainset <- df[trainset_indices, ]
    testset <- df[-trainset_indices, ]
    # Bbuild model 
    # Paste builds formula string and as.formula interprets it as an R formula
    #rpart_model <- rpart(as.formula(paste(class_variable_name,"~.")),data = trainset, method="class")
    model.rf <- randomForest(as.formula(paste(class_variable_name,"~.")),data = trainset, 
                             importance=TRUE, xtest=testset[,-class_col_num],ntree=nTree)
    # Predict on test data
    rpart_predict <- predict(rpart_model,testset[,-class_col_num],type="class")
    test_predictions_rf <- data.frame(testset,model.rf$test$predicted)
    
    # Accuracy
    accuracies[i] <- mean(model.rf$test$predicted==testset[[class_variable_name]])
  }
  return(accuracies)
}

# Calculate average accuracy and std dev over 30 random partitions
accuracy_results <- multiple_runs_rf(trainset,"Target",0.8,30,1000)
mean(accuracy_results)
sd(accuracy_results)
accuracy_results



