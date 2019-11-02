#### ROC charts ####
rm(list=ls())

#### Packages ####
library("ROCR")
library("caret")
library("gbm")
#### Prework ####

# Simply run the code below to create a base GBM model we can play with

set.seed(42)
setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")
repRead<- read.csv("repurchase_training.csv")

# reclassify/merge some of the lower value car_model groupings
recastData <- repRead %>% select(-ID) %>%  
  mutate(car_model = as.factor(ifelse(as.character(car_model) == "model_9" | 
                                        as.character(car_model) == "model_10" | as.character(car_model) == "model_12" | 
                                        as.character(car_model) == "model_14" | as.character(car_model) >= "model_16" & 
                                        as.character(car_model) <= "model_19", "model_0", as.character(car_model))))

# remove some dimensions from dataset for modelling
repurchaseData <- recastData %>% subset(select = -c(age_band,gender,car_segment,car_model,age_of_vehicle_years,non_sched_serv_warr,total_paid_services))

# Get index of predicted variable
class_col_num <- grep("Target",names(repurchaseData))

train_binary = createDataPartition(y = repurchaseData$Target, p = 0.7, list = F)
training_binary = repurchaseData[train_binary, ]
testing_binary = repurchaseData[-train_binary, ]

control_binary <- trainControl(method = "cv",
                               number = 5,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE,
                               allowParallel = TRUE)

training_binary$Target <- as.factor(training_binary$Target)
levels(training_binary$Target) <- c("Single", "Multiple")

testing_binary$Target <- as.factor(testing_binary$Target)
levels(testing_binary$Target) <- c("Single", "Multiple")

# run model, exclude col 1 Target from x
gbm_fit = train(
  x = training_binary[,-class_col_num], 
  y = training_binary$Target, 
  method = "gbm", 
  trControl = control_binary, 
  verbose = T,
  metric = "ROC"
)

#### ROC Graphs ####
#1. Create our prediction objects for training and testing
#2. Get out tpr/fpr to plot
#3. Plot them!

##(1)##

# Create prediction object on the training data
training_binary$predictions = predict(gbm_fit, newdata = training_binary[,-1])
training_binary$probability <- predict(gbm_fit, newdata = training_binary[, 2:9], type = "prob")

train_pred = prediction(training_binary$probability[,1], training_binary$Target)

#And now a prediction object on the testing data
testing_binary$predictions = predict(gbm_fit, newdata = testing_binary[,-1])
testing_binary$probability <- predict(gbm_fit, newdata = testing_binary[, 2:9], type = "prob")

test_pred = prediction(testing_binary$probability[,1], testing_binary$Target)

# Check out our confusion matrix
confusionMatrix(data = testing_binary$predictions, reference = testing_binary$Target,
                mode = "everything", positive="Multiple")

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

#Bonus, some AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc

############  Apply Sample methods to Imbalanced Data  #####################
# As the data has less 0 (single) purchases, we have to apply sample methods to balance the data
# Use SMOTE sampling method using DMwR package
#install.packages('ROSE')
#install.packages('DMwR')
library(DMwR)
library(ROSE)

print('Number of transactions in train dataset before applying sampling methods')
print(table(training_binary$Target))

###################################################################
# SMOTE(Synthetic Minority Over-sampling Technique) Sampling
# formula - relates how our dependent variable acts based on other independent variable.
# data - input data
# perc.over - controls the size of Minority class
# perc.under - controls the size of Majority class
# since my data has less Majority class, increasing it with 200 and keeping the minority class to 100.
smote_sample_train_data <- SMOTE(Target ~ ., data = training_binary[1:9], perc.over = 100, perc.under=200)
print('Number of transactions in train dataset after applying SMOTE sampling method')
print(table(smote_sample_train_data$Target))

# Apply Logistic classifier on balanced data
#---------------------------------------------------------------
# Logistic classifier for SMOTE dataset
# smote_classifier = glm(formula = Target ~ ., family = binomial, data = smote_sample_train_data)
smote_classifier = train(x = smote_sample_train_data[-1], y = smote_sample_train_data$Target, 
                         method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Predicting the test set using SMOTE classifier
#------------------------------------------------------------
#smote_probability_predict = predict(smote_classifier, type = 'response', newdata = testset[-1])
#y_pred_smote = ifelse(smote_probability_predict>0.5, 1, 0)

training_binary$smote_predictions = predict(smote_classifier, newdata = training_binary[-1])
training_binary$smote_probability <- predict(smote_classifier, newdata = training_binary[, 2:9], type = "prob")

smote_train_pred = prediction(training_binary$smote_probability[,1], training_binary$Target)

#And now a prediction object on the testing data
testing_binary$smote_predictions = predict(smote_classifier, newdata = testing_binary[-1])
testing_binary$smote_probability <- predict(smote_classifier, newdata = testing_binary[, 2:9], type = "prob")

smote_test_pred = prediction(testing_binary$smote_probability[,1], testing_binary$Target)

# Check out our confusion matrix
confusionMatrix(data = testing_binary$smote_predictions, reference = testing_binary$Target,
                mode = "everything", positive="Single")
##(2)##

#tpr and fpr for our training
train_tpr_fpr = performance(smote_train_pred, "tpr","fpr") 
train_auc = performance(smote_train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(smote_test_pred, "tpr","fpr")   
test_auc = performance(smote_test_pred, "auc")

##(3)##

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves - SMOTE", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#Bonus, some AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc

#### Sensitivity-Specificity Graphs ####
#1. Plot sensitivity-specificity graphs
#2. Find an optimal probability cutoff

##(1)##

# Here we use the performance function again to extract the sensitivity and specificity graphs from our testing prediction object.

sens = performance(test_pred, "sens")
spec = performance(test_pred, "spec")

# Here we plot a nice graph
plot(sens, 
     main = "Sensitivity Specificity Chart", type = "l", col = "red", lwd = 2, 
     xlim = c(0,1), ylim = c(0,1), 
     ylab = "Values")
axis(side = 1, at = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, 1, 0.1))
plot(spec, add = T, col = "blue", lwd = 2, 
     xlim = c(0,1), ylim = c(0,1)
)

legend("bottomright", legend = c("Sensitivity","Specificity"), col = c("red", "blue"), lty = 1, lwd = 2)
abline(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1), col="gray", lty=3)

##(2)##

# Now let us programatically determine an optimal cutoff

# Firstly get our performance object
test_sens_spec = performance(test_pred, "sens","spec")

#Inspect the element in your environment. See how the x.values are the Specificity, the y.values are Sensitivity at various cutoffs (where the cutoffs are stored in the alpha.values). Therefore we want to find the row (cutoff value) where the sum of sensitivtiy and specificity is maximum

# Firstly let us make a dataframe for convenience
threshold_df = data.frame(cut = test_sens_spec@alpha.values[[1]], 
                          sens = test_sens_spec@x.values[[1]],
                          spec = test_sens_spec@y.values[[1]])

# Take a look at the ?which.max function. We will use it below.
?which.max

# Try it on our sensitivity and specificity columns. Which row was it?
which.max(threshold_df$sens + threshold_df$spec)

#Now we can roll that into a simple subset to get the relevant cutoff value
threshold = threshold_df[which.max(threshold_df$sens + threshold_df$spec), "cut"]
cat("\nProbability threshold is", threshold)


# Alternatively, we could make a column that is the sum of our sens and spec.Run the line below, then open the threshold_df and sort the sens_and_spec column.What row is the largest?

threshold_df$sens_and_spec <- threshold_df$sens + threshold_df$spec

## Final prediction
testing_binary$prediction_cutoff = "Bad"
testing_binary[testing_binary$probability$Good >= threshold, "prediction_cutoff"] = "Good"
testing_binary$prediction_cutoff <- as.factor(testing_binary$prediction_cutoff)
levels(testing_binary$prediction_cutoff) <- c("Bad","Good")

# Now let us take a look at our new confusion matrix

# What do you notice is different? Re-run the first confusionMatrix call above to see. 
# Our accuracy is lower but what about sensitivity and specificity?

confusionMatrix(data = testing_binary$prediction_cutoff, reference = testing_binary$Target,
                mode = "everything", positive="Good")


# Oversampling, as Fraud transactions(1) are having less occurrence, so this Over sampling method will increase the Fraud records untill matches good records 227452
# Here N= 89482*2
over_sample_train_data <- ovun.sample(Target ~ ., data = training_binary, method="over", N=178964)$data
print('Number of transactions in train dataset after applying Over sampling method')
print(table(over_sample_train_data$Target))

# Apply Logistic classifier on balanced data
#---------------------------------------------------------------
# Now we have five different types of inputs which are balanced and ready for prediction.
# We can appply Logistic classifier to all these five datasets and calculate the performance of each.
# based on above:
#   gbm_fit = train(x = training_binary[-1], y = training_binary$Target, 
#                   method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Logistic classifier for Over sampling dataset
# over_classifier = glm(formula = Target ~ ., family = binomial, data = over_sample_train_data)
over_classifier = train(x = over_sample_train_data[-1], y = over_sample_train_data$Target, 
                        method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Predicting the test set using Over sampling classifier
#------------------------------------------------------------
# over_probability_predict = predict(over_classifier, type = 'response', newdata = testing_binary[-1])
# y_pred_over = ifelse(over_probability_predict>0.5, 1, 0)

# Predicting the test set using Over sampling classifier
#------------------------------------------------------------
training_binary$over_predictions = predict(over_classifier, newdata = training_binary[-1])
training_binary$over_probability <- predict(over_classifier, newdata = training_binary[, 2:9], type = "prob")

over_train_pred = prediction(training_binary$over_probability[,2], training_binary$Target)

#And now a prediction object on the testing data
testing_binary$over_predictions = predict(over_classifier, newdata = testing_binary[-1])
testing_binary$over_probability <- predict(over_classifier, newdata = testing_binary[, 2:9], type = "prob")

over_test_pred = prediction(testing_binary$over_probability[,2], testing_binary$Target)

# Check out our confusion matrix
confusionMatrix(data = testing_binary$over_predictions, reference = testing_binary$Target,
                mode = "everything", positive="Single")
##(2)##

#tpr and fpr for our training
train_tpr_fpr = performance(over_train_pred, "tpr","fpr")
train_auc = performance(over_train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(over_test_pred, "tpr","fpr")
test_auc = performance(over_test_pred, "auc")

##(3)##

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves - Oversampling", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("topleft", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#Bonus, some AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc

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

###################################################################
# Undersampling,as Fraud transactions(1) are having less occurrence, so this Under sampling method will descrease the Good records untill matches Fraud records, But, you see that we’ve lost significant information from the sample. 
# Here N= 2454*2
under_sample_train_data <- ovun.sample(Target ~ ., data = training_binary[1:9], method="under", N=4908)$data
print('Number of transactions in train dataset after applying Under sampling method')
print(table(under_sample_train_data$Target))

# Apply Logistic classifier on balanced data
#---------------------------------------------------------------
# Logistic classifier for Under sampling dataset
# under_classifier = glm(formula = Target ~ ., family = binomial, data = under_sample_train_data)
under_classifier = train(x = under_sample_train_data[-1], y = under_sample_train_data$Target, 
                         method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Predicting the test set using Under sampling classifier
#------------------------------------------------------------
# under_probability_predict = predict(under_classifier, type = 'response', newdata = testset[-1])
# y_pred_under = ifelse(under_probability_predict>0.5, 1, 0)

training_binary$under_predictions = predict(under_classifier, newdata = training_binary[-1])
training_binary$under_probability <- predict(under_classifier, newdata = training_binary[, 2:9], type = "prob")

under_train_pred = prediction(training_binary$under_probability[,1], training_binary$Target)

#And now a prediction object on the testing data
testing_binary$under_predictions = predict(under_classifier, newdata = testing_binary[-1])
testing_binary$under_probability <- predict(under_classifier, newdata = testing_binary[, 2:9], type = "prob")

under_test_pred = prediction(testing_binary$under_probability[,1], testing_binary$Target)

# Check out our confusion matrix
confusionMatrix(data = testing_binary$under_predictions, reference = testing_binary$Target,
                mode = "everything", positive="Single")
##(2)##

#tpr and fpr for our training
train_tpr_fpr = performance(under_train_pred, "tpr","fpr")
train_auc = performance(under_train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(under_test_pred, "tpr","fpr")
test_auc = performance(under_test_pred, "auc")

##(3)##

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves - Undersampling", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("topleft", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#Bonus, some AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc

###################################################################
# Mixed Sampling, apply both under sampling and over sampling on this imbalanced data
training_binary$over_predictions <- NULL
training_binary$over_probability <- NULL
training_binary$under_predictions <- NULL
training_binary$under_probability <- NULL
training_binary$both_predictions <- NULL
training_binary$both_probability <- NULL

both_sample_train_data <- ovun.sample(Target ~ ., data = training_binary, method="both", p=0.5, seed=1, N=91936)$data
print('Number of transactions in train dataset after applying Mixed sampling method')
print(table(both_sample_train_data$Target))

# Apply Logistic classifier on balanced data
#---------------------------------------------------------------
# Logistic classifier for Mixed sampling dataset
# both_classifier = glm(formula = Target ~ ., family = binomial, data = both_sample_train_data)
both_classifier = train(x = both_sample_train_data[-1], y = both_sample_train_data$Target, 
                        method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Predicting the test set using Mixed sampling classifier
#------------------------------------------------------------
#both_probability_predict = predict(both_classifier, type = 'response', newdata = testset[-1])
#y_pred_both = ifelse(both_probability_predict>0.5, 1, 0)

training_binary$both_predictions = predict(both_classifier, newdata = training_binary[-1])
training_binary$both_probability <- predict(both_classifier, newdata = training_binary[, 2:9], type = "prob")

both_train_pred = prediction(training_binary$both_probability[,2], training_binary$Target)

#And now a prediction object on the testing data
testing_binary$both_predictions = predict(both_classifier, newdata = testing_binary[-1])
testing_binary$both_probability <- predict(both_classifier, newdata = testing_binary[, 2:9], type = "prob")

both_test_pred = prediction(testing_binary$both_probability[,2], testing_binary$Target)

# Check out our confusion matrix
confusionMatrix(data = testing_binary$both_predictions, reference = testing_binary$Target,
                mode = "everything", positive="Single")
##(2)##

#tpr and fpr for our training
train_tpr_fpr = performance(both_train_pred, "tpr","fpr") 
train_auc = performance(both_train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(both_test_pred, "tpr","fpr")   
test_auc = performance(both_test_pred, "auc")

##(3)##

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves - Mixed-sampling", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#Bonus, some AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc


###################################################################
# ROSE Sampling, this helps us to generate data synthetically. It generates artificial datas instead of dulicate data.
rose_sample_train_data <- ROSE(Target ~ ., data = training_binary,  seed=111)$data
print('Number of transactions in train dataset after applying ROSE sampling method')
print(table(rose_sample_train_data$Target))

# Apply Logistic classifier on balanced data
#---------------------------------------------------------------
# Logistic classifier for ROSE sampling dataset
# rose_classifier = glm(formula = Target ~ ., family = binomial, data = rose_sample_train_data)
rose_classifier = train(x = rose_sample_train_data[-1], y = rose_sample_train_data$Target, 
                        method = "gbm", trControl = control_binary, verbose = T, metric = "ROC")

# Predicting the test set using ROSE classifier
#------------------------------------------------------------
rose_probability_predict = predict(rose_classifier, type = 'response', newdata = testset[-1])
y_pred_rose = ifelse(rose_probability_predict>0.5, 1, 0)




training_binary$over_predictions <- NULL
training_binary$over_probability <- NULL
training_binary$under_predictions <- NULL
training_binary$under_probability <- NULL
training_binary$both_predictions <- NULL
training_binary$both_probability <- NULL
