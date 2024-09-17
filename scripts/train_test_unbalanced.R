#load needed packages
library(randomForest)
library(gbm)
library(caret)
library(tidyverse)
library(ggplot2)
library(xgboost)
library(ROSE)
library(pROC)

#read in preprocessed datasets
numerical_df <- read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/final_data_numerical.csv")[,-1]
categorical_df <- read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/final_data_categorical.csv")[,-1]

categorical_df$DC1_or_DC2 <- as.factor(categorical_df$DC1_or_DC2)
numerical_df$DC1_or_DC2 <- as.factor(numerical_df$DC1_or_DC2)

#Partition data for training and testing
set.seed(222)
ind <- sample(2, nrow(numerical_df), replace = TRUE, prob = c(0.8, 0.2))
num_train <- numerical_df[ind==1,]
num_test <- numerical_df[ind==2,]
cat_train <- categorical_df[ind==1,]
cat_test <- categorical_df[ind==2,]

#convert DC1_or_DC2 to factor

#train random forest on numeric train
rf_num <- randomForest(DC1_or_DC2~., data=num_train[,-1], proximity=TRUE)
rf_cat <- randomForest(DC1_or_DC2~., data=cat_train[,-1], proximity=TRUE)
saveRDS(rf_num, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/rf_num.rds")
saveRDS(rf_cat, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/rf_cat.rds")

#prediction and assessment for rf_num
print("Testing rf_num")
p_test_num <- predict(rf_num, num_test[,-1])
confusionMatrix(p_test_num, num_test[,-1]$DC1_or_DC2)
rf_test_num_prob <- predict(rf_num, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, rf_test_num_prob)

#prediction and assessment for rf_cat
print("Testing rf_cat")
p_test_cat <- predict(rf_cat, cat_test[,-1])
confusionMatrix(p_test_cat, cat_test[,-1]$DC1_or_DC2)
rf_test_cat_prob <- predict(rf_cat, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, rf_test_cat_prob)


#trying gbm with caret
tc = trainControl(method = "cv", number=10)
gbm_num = train(DC1_or_DC2 ~., data=num_train[,-1], method="gbm", trControl=tc)
gbm_cat = train(DC1_or_DC2 ~., data=cat_train[,-1], method="gbm", trControl=tc)

#Test gbm models
print("Testing gbm_num")
p_test_gbm_num <- predict(gbm_num, num_test[,-1])
confusionMatrix(p_test_gbm_num, num_test[,-1]$DC1_or_DC2)
gbm_test_num_prob <- predict(gbm_num, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, gbm_test_num_prob)

print("Testing gbm_cat")
p_test_gbm_cat <- predict(gbm_cat, cat_test[,-1])
confusionMatrix(p_test_gbm_cat, cat_test[,-1]$DC1_or_DC2)
gbm_test_cat_prob <- predict(gbm_cat, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, gbm_test_cat_prob)

#save models so far
saveRDS(gbm_num, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_num.rds")
saveRDS(gbm_cat, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat.rds")
write.csv(num_test, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_test_unbalanced.csv")
write.csv(num_train, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_train_unbalanced.csv")
write.csv(cat_test, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_test_unbalanced.csv")
write.csv(cat_train, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_train_unbalanced.csv")