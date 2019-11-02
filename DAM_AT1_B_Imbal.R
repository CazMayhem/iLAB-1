library("ROCR")
library("glmnet")
library("corrplot")

rm(list=ls())
setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")
repRead<- read.csv("repurchase_training.csv")

#repRead$pred = round(predict(default.glm, repRead, type = "response") , digits = 4)
#head(repRead)
#str(repRead)
#-----------------------------------------------------------------------

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

# Mega important step, convert Target to factor - or ROC graphs will report near 0 AUC - not good!
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

# assume that the optimum probability threshold is 0.5, create the class prediction - our target is 0 customer has only purchased 1
testset$prediction = "1"
testset[testset$probability < 0.5, "prediction"] = "0"

# Have a look at the data
# head(testset)

# confusion matrix
confusionMatrix(table(predicted=testset$prediction,actual=testset$Target))
table_mat <- table(predicted=testset$prediction,actual=testset$Target)

#------------------------------------------------------------------------
# https://www.guru99.com/r-generalized-linear-model.html
#------------------------------------------------------------------------

# To check the accuracy of this model using ROC curve.
accuracy_Test <- mean(testset$prediction==testset$Target)
accuracy_Test

#confusion matrix
cfm <- table(predicted=testset$prediction,actual=testset$Target)
cfm

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
trainset$probability <- predict(rep_glm, newdata = trainset[2:9], type = "response")

train_pred = prediction(trainset$prediction, trainset$Target)

#And now a prediction object on the testing data
testset$predictions = predict(rep_glm, newdata = testset[-1])
testset$probability <- predict(rep_glm, newdata = testset[2:9], type = "response")

test_pred = prediction(testset$probability, testset$Target)

# Check out our confusion matrix
confusionMatrix(data = testset$predictions, reference = testset$Target,
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
plot(test_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
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

# Receiver Operating Characteristic ROC curve - shows the true positive rate (i.e., recall) against the false positive rate. 
# ROC curve plots sensitivity (recall) versus 1-specificity
library("ROCR")
ROCRpred <- prediction(testset$probability, testset$Target)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

#################################Apply Sample methods to Imbalanced Data#######################

rep_glm = glm(formula = Target ~ .,
              data = trainset,
              family = "binomial")

# As the data has less Fraud transactions(less than 1%), we have to apply sample methods to balance the data
# We applied Over, Upper, Mixed(both) and ROSE sampling methods using ROSE package and SMOTE sampling method using DMwR package
#install.packages('ROSE')
#install.packages('DMwR')
library(DMwR)
library(ROSE)

print('Number of transactions in train dataset before applying sampling methods')
print(table(trainset$Target))

# Oversampling, as Fraud transactions(1) are having less occurrence, so this Over sampling method will increase the Fraud records untill matches good records 227452
# Here N= 89482*2
over_sample_train_data <- ovun.sample(Target ~ ., data = trainset, method="over", N=178964)$data
print('Number of transactions in train dataset after applying Over sampling method')
print(table(over_sample_train_data$Target))

# Undersampling,as Fraud transactions(1) are having less occurrence, so this Under sampling method will descrease the Good records untill matches Fraud records, But, you see that we’ve lost significant information from the sample. 
# Here N= 2454*2
under_sample_train_data <- ovun.sample(Target ~ ., data = trainset, method="under", N=4908)$data
print('Number of transactions in train dataset after applying Under sampling method')
print(table(under_sample_train_data$Target))

# Mixed Sampling, apply both under sampling and over sampling on this imbalanced data
both_sample_train_data <- ovun.sample(Target ~ ., data = trainset, method="both", p=0.5, seed=222, N=91936)$data
print('Number of transactions in train dataset after applying Mixed sampling method')
print(table(both_sample_train_data$Target))

# ROSE Sampling, this helps us to generate data synthetically. It generates artificial datas instead of dulicate data.
rose_sample_train_data <- ROSE(Target ~ ., data = trainset,  seed=111)$data
print('Number of transactions in train dataset after applying ROSE sampling method')
print(table(rose_sample_train_data$Target))

# SMOTE(Synthetic Minority Over-sampling Technique) Sampling
# formula - relates how our dependent variable acts based on other independent variable.
# data - input data
# perc.over - controls the size of Minority class
# perc.under - controls the size of Majority class
# since my data has less Majority class, increasing it with 200 and keeping the minority class to 100.
smote_sample_train_data <- SMOTE(Target ~ ., data = trainset, perc.over = 100, perc.under=200)
print('Number of transactions in train dataset after applying SMOTE sampling method')
print(table(smote_sample_train_data$Target))

###################Apply Logistic classifier on balanced data###########################

# Now we have five different types of inputs which are balanced and ready for prediction.
# We can appply Logistic classifier to all these five datasets and calculate the performance of each.

# Logistic classifier for Over sampling dataset
over_classifier = glm(formula = Target ~ ., family = binomial, data = over_sample_train_data)

# Logistic classifier for Under sampling dataset
under_classifier = glm(formula = Target ~ ., family = binomial, data = under_sample_train_data)

# Logistic classifier for Mixed sampling dataset
both_classifier = glm(formula = Target ~ ., family = binomial, data = both_sample_train_data)

#Logistic classifier for ROSE sampling dataset
rose_classifier = glm(formula = Target ~ ., family = binomial, data = rose_sample_train_data)

# Logistic classifier for SMOTE dataset
smote_classifier = glm(formula = Target ~ ., family = binomial, data = smote_sample_train_data)

#########################Prediction on test set#############################

# Prediction on test set using sampling classifiers

# Predicting the test set using Over sampling classifier
over_probability_predict = predict(over_classifier, type = 'response', newdata = testset[-1])
y_pred_over = ifelse(over_probability_predict>0.5, 1, 0)

# Predicting the test set using Under sampling classifier
under_probability_predict = predict(under_classifier, type = 'response', newdata = testset[-1])
y_pred_under = ifelse(under_probability_predict>0.5, 1, 0)

# Predicting the test set using Mixed sampling classifier
both_probability_predict = predict(both_classifier, type = 'response', newdata = testset[-1])
y_pred_both = ifelse(both_probability_predict>0.5, 1, 0)

# Predicting the test set using ROSE classifier
rose_probability_predict = predict(rose_classifier, type = 'response', newdata = testset[-1])
y_pred_rose = ifelse(rose_probability_predict>0.5, 1, 0)

# Predicting the test set using SMOTE classifier
smote_probability_predict = predict(smote_classifier, type = 'response', newdata = testset[-1])
y_pred_smote = ifelse(smote_probability_predict>0.5, 1, 0)

############################ROC Curve###########################

# roc.curve function from ROSE package returns the ROC curve and AUC value.
# We can see the AUC value by making the plotit as FALSE and print the curve.
# It takes dependent variable as the first parameter and the class to be evaluated
# plotit is logical for plotting the ROC curve. color of the curve can be given in col.

# ROC curve of over sampling data
roc_over <- roc.curve(testset$Target, y_pred_over)
print(roc_over)
# ROC curve of Under sampling data
roc_under <- roc.curve(testset$Target, y_pred_under)
print(roc_under)
# ROC curve of both sampling data
roc_both <- roc.curve(testset$Target, y_pred_both)
print(roc_both)
# ROC curve of ROSE sampling data
roc_rose <- roc.curve(testset$Target, y_pred_rose)
print(roc_rose)
# ROC curve of SMOTE sampling data
roc_smote <- roc.curve(testset$Target, y_pred_smote)
print(roc_smote)



# Step 8) Improve the model
#------------------------------------------------------------------------
# You can try to add non-linearity to the model with the interaction between
# - age and hours.per.week
# - gender and hours.per.week.
# You need to use the score test to compare both model
# formula_2 <- income~age: hours.per.week + gender: hours.per.week + .

# Reset observations to training and testing sets
trainset <- recastData[trainset_indices, ]
testset <- recastData[-trainset_indices, ]

formula_2 <- Target ~ num_dealers_visited: car_model + age_of_vehicle_years: car_model + .
logit_2 <- glm(formula_2, data = trainset, family = 'binomial')
predict_2 <- predict(logit_2, testset[-1], type = 'response')
table_mat_2 <- table(predicted=ifelse(predict_2>0.5, 1, 0), actual=testset$Target)
precision_2 <- precision(table_mat_2)
recall_2 <- recall(table_mat_2)
accuracy_2 <- sum(diag(table_mat_2)) / sum(table_mat_2)
f1_2 <- 2 * ((precision_2 * recall_2) / (precision_2 + recall_2))
f1_2

