#### Gradient Boosting Machine ####
for (package in c("tidyverse","lubridate","ggplot2","scales","car","caret","dplyr","gbm","ROCR","randomForest")) {
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
  if (!package %in% .packages()) {
    library(package, character.only = TRUE)
  }
}
# This exercise involves building a gbm model 
rm(list=ls())
#library(gbm)

#set working directory if needed (modify path as needed)
setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")

# Read in and format our data
repRead <- read.csv("repurchase_training.csv")

# reclassify/merge some of the lower value car_model groupings
recastData <- repRead %>% select(-ID) %>%  
  mutate(car_model = as.factor(ifelse(as.character(car_model) == "model_9" | 
                                        as.character(car_model) == "model_10" | as.character(car_model) == "model_12" | 
                                        as.character(car_model) == "model_14" | as.character(car_model) >= "model_16" & 
                                        as.character(car_model) <= "model_19", "model_0", as.character(car_model))))

# Drop the non statistically significant variables from our earlier EDA - age_band,gender,car_segment,car_model,age_of_vehicle_years,non_sched_serv_paid,total_paid_services
repurchData <- recastData %>% subset(select = -c(age_band,car_segment))

# Check data balance/imbalance
table_1 <- table(repurchData$Target)
table_1
sprintf("%.1f%%",round(prop.table(table_1)*100, 3))

# Create training and test sets. This process should be familiar by now
trainset_size <- floor(0.80 * nrow(repurchData))
set.seed(42) 

trainset_indices <- sample(seq_len(nrow(repurchData)), size = trainset_size)
training <- repurchData[trainset_indices, ]
testset <- repurchData[-trainset_indices, ]

#training$Target <- as.factor(training$Target)
#levels(training$Target) <- c("Single", "Multiple")

#testset$Target <- as.factor(testset$Target)
#levels(testset$Target) <- c("Single", "Multiple")

# Checks
#nrow(training)
#nrow(testset)
#nrow(wine_data)

#### Train the model ####

# Defining some parameters

gbm_depth = 5       #maximum nodes per tree
gbm_n.min = 15      #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.01  #learning rate
cores_num = 10      #number of cores
gbm_cv_folds=10      #number of cross-validation folds to perform
num_trees = 1000    #Number of iterations

start <- proc.time()

# fit initial model
gbm_clf = gbm(training$Target~.,
                  data=training[, -1],
                  distribution='bernoulli', #binary response
                  n.trees=num_trees,
                  interaction.depth= gbm_depth,
                  n.minobsinnode = gbm_n.min, 
                  shrinkage=gbm_shrinkage, 
                  cv.folds=gbm_cv_folds,
                  verbose = TRUE, #print the preliminary output
                  n.cores = cores_num
)

end <- proc.time() - start
end_time <- as.numeric((paste(end[3])))
end_time

# Estimate the optimal number of iterations (when will the model stop improving)
# The black is the training deviance dropping whilst the green is the test.
best.iter = gbm.perf(gbm_clf, method = "cv")
print(best.iter)

# Gives the variable importance in a graph
par(cex=0.7,mar=c(5, 12, 2, 3))
summary(gbm_clf, n.trees=best.iter, cBars = 16, las = 2, main = "Variable Relative Importance")

# OR just as a table
summary(gbm_clf)

saveTest <- testset

testset <- saveTest

# Let us get our estimates
testset$probability = predict(gbm_clf, testset, n.trees = best.iter, type = "response")
testset$prediction = 0
# Modify the probability threshold to see if you can get a better accuracy
testset[testset$probability >= 0.5, "prediction"] = 1

test_pred = prediction(testset$prediction, testset$Target)

# Confusion matrix
cfm <- table(predicted=testset$prediction, actual=testset$Target)
cfm

# Accuracy
#sum(diag(cfm)) / nrow(testset)
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

# Create prediction object on the training data - assume that the optimum probability threshold is 0.5, our target is 0 customer has only purchased 1
training$probability = predict(gbm_clf, training, n.trees = best.iter, type = "response")
training$prediction = 0
# Modify the probability threshold to see if you can get a better accuracy
training[training$probability >= 0.5, "prediction"] = 1

train_pred = prediction(training$prediction, training$Target)

# convert target & prediction to Factors for ROC
testset$Target = factor(testset$Target)
testset$prediction = factor(testset$prediction)

# Check out our confusion matrix
confusionMatrix(data = testset$prediction, reference = testset$Target,
                mode = "everything", positive="0")

# ROC and AUC our models performance
# -------------------------------------------------------------

#tpr and fpr for our training
train_tpr_fpr = performance(train_pred, "tpr","fpr")
train_auc = performance(train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr = performance(test_pred, "tpr","fpr")
test_auc = performance(test_pred, "auc")

# Plot the tpr and fpr gains chart ROC for both testing and training data
par(cex=1,mar=c(5, 3, 2, 2))
plot(test_tpr_fpr, main="Testing and Training ROC Curves - GBM", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# Area under the ROC curve - training data
train_auc_n = unlist(slot(train_auc, "y.values"))
str_c("Training data AUC = ", round(train_auc_n,5))

# Area under the ROC curve - testing data
test_auc_n = unlist(slot(test_auc, "y.values"))
str_c("Testing data AUC = ", round(test_auc_n,5))
