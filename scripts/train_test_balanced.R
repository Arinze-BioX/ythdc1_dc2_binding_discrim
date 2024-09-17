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


#Balancing the data
# Run undersampled function
downsample_multiclass<- function(df, test_variable){
    classes=names(sort(table(df[,names(df) == test_variable])))
    rare_class <- classes[1]
    n=length(classes)
    new_df <- df[df[,names(df) == test_variable] == rare_class,]
    i=2
    while(i < (n+1)){
        test_df <- df[df[,names(df) == test_variable] %in% c(rare_class, classes[i]),]
        assign("test_df", test_df, envir=globalenv())
        assign("test_variable", test_variable, envir=globalenv())
        balanced_tmp <- ovun.sample(DC1_or_DC2 ~ ., data = test_df, 
        method = "under", p = 0.5, seed = 222)$data
        new_df <- rbind(new_df, balanced_tmp[balanced_tmp[,names(balanced_tmp) == test_variable] == classes[i],])
        print("third")
        print(dim(new_df))
        i=i+1
    }
    return(new_df)
}

balanced_num <- downsample_multiclass(numerical_df, "DC1_or_DC2")
balanced_cat <- downsample_multiclass(categorical_df, "DC1_or_DC2")


#now split balanced datset
ind <- sample(2, nrow(balanced_num), replace = TRUE, prob = c(0.8, 0.2))
num_train <- balanced_num[ind==1,]
num_test <- balanced_num[ind==2,]
ind <- sample(2, nrow(balanced_cat), replace = TRUE, prob = c(0.8, 0.2))
cat_train <- balanced_cat[ind==1,]
cat_test <- balanced_cat[ind==2,]

#convert DC1_or_DC2 to factor

#train random forest on balanced numeric train
rf_num_balanced <- randomForest(DC1_or_DC2~., data=num_train[,-1], proximity=TRUE)
rf_cat_balanced <- randomForest(DC1_or_DC2~., data=cat_train[,-1], proximity=TRUE)

#prediction and assessment for rf_num
p_train_num <- predict(rf_num_balanced, num_train[,-1])
confusionMatrix(p_train_num, num_train[,-1]$DC1_or_DC2)

p_test_num <- predict(rf_num_balanced, num_test[,-1])
confusionMatrix(p_test_num, num_test[,-1]$DC1_or_DC2)


#prediction and assessment for rf_cat
p_train_cat <- predict(rf_cat_balanced, cat_train[,-1])
confusionMatrix(p_train_cat, cat_train[,-1]$DC1_or_DC2)

p_test_cat <- predict(rf_cat_balanced, cat_test[,-1])
confusionMatrix(p_test_cat, cat_test[,-1]$DC1_or_DC2)


#trying gbm with caret
tc = trainControl(method = "cv", number=10)
gbm_num_balanced = train(DC1_or_DC2 ~., data=num_train[,-1], method="gbm", trControl=tc)
gbm_cat_balanced = train(DC1_or_DC2 ~., data=cat_train[,-1], method="gbm", trControl=tc)

#Test gbm models
p_test_gbm_num <- predict(gbm_num_balanced, num_test[,-1])
confusionMatrix(p_test_gbm_num, num_test[,-1]$DC1_or_DC2)

p_test_gbm_cat <- predict(gbm_cat_balanced, cat_test[,-1])
confusionMatrix(p_test_gbm_cat, cat_test[,-1]$DC1_or_DC2)



#trying xgboost on balanced dataset
numberOfClasses <- length(unique(num_train[,-1]$DC1_or_DC2))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
                   
nround    <- 50 # number of XGBoost rounds
#cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
#cv_model <- xgb.cv(params = xgb_params,
#                   data = train_matrix, 
#                   nrounds = nround,
#                   nfold = cv.nfold,
#                   verbose = FALSE,
#                   prediction = TRUE)
num_trainp<-as.matrix(num_train[,-c(1,2)])
num_lab<-as.numeric(num_train$DC1_or_DC2)-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
train_matrix <- xgb.DMatrix(data = num_trainp, label = num_lab)
xgb_num <- xgboost(data = train_matrix, params = xgb_params, nrounds=nround)

#and test model
# Predict hold-out test set
num_testp<-as.matrix(num_test[,-c(1,2)])
num_test_lab<-as.numeric(num_test$DC1_or_DC2)-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
test_matrix <- xgb.DMatrix(data = num_testp, label = num_test_lab)
test_pred <- predict(xgb_num, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = num_test_lab + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")



#do same for categorical
cat_trainp<-as.matrix(cat_train[,-c(1,2)])
cat_lab<-as.numeric(cat_train$DC1_or_DC2)-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
train_matrix <- xgb.DMatrix(data = cat_trainp, label = cat_lab)
xgb_cat <- xgboost(data = train_matrix, params = xgb_params, nrounds=nround)

#and test model
# Predict hold-out test set
cat_testp<-as.matrix(cat_test[,-c(1,2)])
cat_test_lab<-as.numeric(cat_test$DC1_or_DC2)-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
test_matrix <- xgb.DMatrix(data = cat_testp, label = cat_test_lab)
test_pred <- predict(xgb_cat, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = cat_test_lab + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")



#save all models
#save models so far
saveRDS(rf_num_balanced, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/rf_num_balanced.rds")
saveRDS(rf_cat_balanced, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/rf_cat_balanced.rds")
saveRDS(gbm_num_balanced, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_num_balanced.rds")
saveRDS(gbm_cat_balanced, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat_balanced.rds")
saveRDS(xgb_cat, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/xgb_cat.rds")
saveRDS(xgb_num, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/xgb_num.rds")
write.csv(num_test, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_test_balanced.csv")
write.csv(num_train, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_train_balanced.csv")
write.csv(cat_test, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_test_balanced.csv")
write.csv(cat_train, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_train_balanced.csv")

num_test<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_test_balanced.csv")
num_train<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_train_balanced.csv")
cat_test<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_test_balanced.csv")
cat_train<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_train_balanced.csv")

#rf_cat_balanced<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/rf_cat_balanced.rds")
#gbm_num_balanced<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_num_balanced.rds")
#gbm_cat_balanced<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat_balanced.rds")
#xgb_cat<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/xgb_cat.rds")
#xgb_num<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/xgb_num.rds")

#Results:
#rf_num:            Accuracy=0.7739, AUC=0.8191
#rf_num_balanced:   Accuracy=0.666, AUC=0.8246
#rf_cat:            Accuracy=0.7761, AUC=0.8046
#rf_cat_balanced:   Accuracy=0.6723, AUC=0.8208
#gbm_cat:           Accuracy=0.773 , AUC=0.8376
#gbm_cat_balanced:  Accuracy=0.6744 , AUC=0.8368
#gbm_num:           Accuracy=0.7741 , AUC=0.8353
#gbm_num_balanced   Accuracy=0.6667, AUC=0.8331
#xgboost_num:       Accuracy=0.6623 , AUC=
#xgboos_cat:        Accuracy=0.6699 , AUC=
#I'll plot ROC and get important features for gbm_cat and gbm_cat balanced

#CALCULATING AUC FOR THE BALANCED MODELS
#now for rf_num_balanced
rf_test_num <- predict(rf_num_balanced, num_test[,-1])
confusionMatrix(rf_test_num, num_test$DC1_or_DC2)
rf_test_num_prob <- predict(rf_num_balanced, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, rf_test_num_prob)

#now for rf_cat_balanced
rf_test_cat <- predict(rf_cat_balanced, cat_test[,-1])
confusionMatrix(rf_test_cat, cat_test$DC1_or_DC2)
rf_test_cat_prob <- predict(rf_cat_balanced, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, rf_test_cat_prob)

#now for gbm_cat_balanced
gbm_test_cat <- predict(gbm_cat_balanced, cat_test[,-1])
confusionMatrix(gbm_test_cat, cat_test$DC1_or_DC2)
gbm_test_cat_prob <- predict(gbm_cat_balanced, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, gbm_test_cat_prob)

#now for gbm_num_balanced
gbm_test_num <- predict(gbm_num_balanced, num_test[,-1])
confusionMatrix(gbm_test_num, num_test$DC1_or_DC2)
gbm_test_num_prob <- predict(gbm_num_balanced, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, gbm_test_num_prob)

#now for xgboost_num
num_testp<-as.matrix(num_test[,-c(1,2)])
num_test_lab<-as.numeric(factor(num_test$DC1_or_DC2, levels=c("Both_DC1_DC2","DC1_only","DC2_only")))-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
test_matrix <- xgb.DMatrix(data = num_testp, label = num_test_lab)
test_pred <- predict(xgb_num, newdata = test_matrix, type="prob")
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = num_test_lab + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

#now for xgboost_cat
cat_testp<-as.matrix(cat_test[,-c(1,2)])
cat_test_lab<-as.numeric(factor(cat_test$DC1_or_DC2, levels=c("Both_DC1_DC2","DC1_only","DC2_only")))-1   #Levels: Both_DC1_DC2 is 0, DC1_only is 1, DC2_only is 2
test_matrix <- xgb.DMatrix(data = cat_testp, label = cat_test_lab)
test_pred <- predict(xgb_cat, newdata = test_matrix, type="prob")
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = cat_test_lab + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")





#CALCULATING AUC FOR THE UNBALANCED MODELS
#Calculating AUC for rf num
rf_test_num <- predict(rf_num, num_test[,-1])
confusionMatrix(rf_test_num, num_test$DC1_or_DC2)
rf_test_num_prob <- predict(rf_num, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, rf_test_num_prob)

#now for rf_cat
rf_test_cat <- predict(rf_cat, cat_test[,-1])
confusionMatrix(rf_test_cat, cat_test$DC1_or_DC2)
rf_test_cat_prob <- predict(rf_cat, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, rf_test_cat_prob)

#now for gbm_cat
gbm_test_cat <- predict(gbm_cat, cat_test[,-1])
confusionMatrix(gbm_test_cat, cat_test$DC1_or_DC2)
gbm_test_cat_prob <- predict(gbm_cat, cat_test[,-1], type="prob")
multiclass.roc(cat_test$DC1_or_DC2, gbm_test_cat_prob)

#now for gbm_num
gbm_test_num <- predict(gbm_num, num_test[,-1])
confusionMatrix(gbm_test_num, num_test$DC1_or_DC2)
gbm_test_num_prob <- predict(gbm_num, num_test[,-1], type="prob")
multiclass.roc(num_test$DC1_or_DC2, gbm_test_num_prob)
