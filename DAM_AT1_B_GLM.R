library("ROCR")
library("glmnet")
library("corrplot")

rm(list=ls())
setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")
repRead<- read.csv("repurchase_training.csv")
repurchaseCSV <- read.csv("repurchase_training.csv")
repurchValidCSV <- read.csv("repurchase_validation.csv")
str(repurchaseCSV)
str(repurchValidCSV)
# reclassify/merge some of the lower value car_model groupings
recastData <- repRead %>% select(-ID) %>%
  mutate(car_model = as.factor(ifelse(as.character(car_model) == "model_9" | 
                                        as.character(car_model) == "model_10" | as.character(car_model) == "model_12" | 
                                        as.character(car_model) == "model_14" | as.character(car_model) >= "model_16" & 
                                        as.character(car_model) <= "model_19", "model_0", as.character(car_model))))

# remove some dimensions from dataset for modelling - car_model,
recastData <- recastData %>% subset(select = -c(age_band,gender,car_segment,age_of_vehicle_years,non_sched_serv_warr,total_paid_services))

set.seed(42) 
trainset_size <- floor(0.70 * nrow(recastData))
trainset_indices <- sample(seq_len(nrow(recastData)), size = trainset_size)

# Assign observations to training and testing sets
trainset <- recastData[trainset_indices, ]
testset <- recastData[-trainset_indices, ]

# MEGA important step, convert Target to factor - or ROC graphs will report near 0 AUC - not good!
trainset$Target <- as.factor(trainset$Target)
testset$Target  <- as.factor(testset$Target)

# Rowcounts to check
#nrow(trainset)
#nrow(testset)
#nrow(recastData)

# Let's start by throwing all the variables into the logistic regression
#-----------------------------------------------------------------------
#library("glmnet")
rep_glm = glm(formula = Target ~ .,
              data = trainset,
              family = "binomial")
summary(rep_glm)

# We may have multi-collinearity present.  Use the 'pairs' plot function to check
# pairs(recast_data[, c("age_of_vehicle_years","total_services","annualised_mileage","num_serv_dealer_purchased")])

# Or a more visual correlation plot - first have to grab the numerical variables
num_vars <- unlist(lapply(trainset, is.numeric))  
train_nums <- trainset[ , num_vars]
train_corr <- cor(train_nums)

#corrplot(train_corr, method = "ellipse", tl.cex = 0.8, cl.cex=0.8, number.cex=0.8)

#-----------------------------------------------------------------------
#### Create probabilities and predictions - Imbalanced Data         ####
#-----------------------------------------------------------------------

# Add the probabilities to the testing data
testset$probability = predict(rep_glm, newdata = testset[-1], type = "response")

# assume that the optimum probability threshold is 0.5, create the class prediction - our target is 1 customer has purchased more than once
testset$prediction = "1"
testset[testset$probability < 0.5, "prediction"] = "0"

#------------------------------------------------------------------------
# Test Accuracy, Precision, Recall, F1 - with help from
# https://www.guru99.com/r-generalized-linear-model.html
#------------------------------------------------------------------------

#confusion matrix
#confusionMatrix(table(predicted=testset$prediction,actual=testset$Target))
cfm <- table(predicted=testset$prediction, actual=testset$Target)
cfm

# To check the accuracy of this model using ROC curve.
accuracy_Test <- mean(testset$prediction==testset$Target)
accuracy_Test

#Precision = TP/(TP+FP)
precision <- cfm[1,1]/(cfm[1,1]+cfm[1,2])
precision

#Recall = TP/(TP+FN)
recall <- cfm[1,1]/(cfm[1,1]+cfm[2,1])
recall

#F1 score based on the precision and recall. The  is a harmonic mean of these two metrics, meaning it gives more weight to the lower values.
f1 <- 2*(precision*recall/(precision+recall))
f1

#### ROC Graphs ####
#1. Create our prediction objects for training and testing
#2. Get out tpr/fpr to plot
#3. Plot them!

# Create prediction object on the training data - assume that the optimum probability threshold is 0.5, our target is 0 customer has only purchased 1
trainset$predictions = predict(rep_glm, newdata = trainset[-1])
trainset$probability <- predict(rep_glm, newdata = trainset[2:10], type = "response")

train_pred = prediction(trainset$probability, trainset$Target)

#And now a prediction object on the testing data
testset$predictions = predict(rep_glm, newdata = testset[-1])
testset$probability <- predict(rep_glm, newdata = testset[2:10], type = "response")

test_pred = prediction(testset$probability, testset$Target)

testset$predictions  <- as.factor(testset$predictions)

# Check out our confusion matrix
confusionMatrix(data = factor(testset$prediction), reference = testset$Target,
                mode = "everything", positive="0")

##(2)##

#tpr and fpr for our training
train_tpr_fpr = performance(train_pred, "tpr","fpr")
train_auc = performance(train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(test_pred, "tpr","fpr")
test_auc = performance(test_pred, "auc")

##(3)##

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves (glm)", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# Area under the ROC curve - training data
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve - testing data
test_auc = unlist(slot(test_auc, "y.values"))
test_auc

