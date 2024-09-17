#load needed packages
library(tidyverse)
library(ggplot2)
library(caret)
library(multiROC)
library(stringr)


#read in test files
num_test<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/num_test_balanced.csv")[,-1]
cat_test<-read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/cat_test_balanced.csv")[,-1]
gbm_cat_balanced<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat_balanced.rds")
gbm_cat<-readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat.rds")


#test
prob_gbm_cat <- predict(gbm_cat, num_test[,-1], type="prob")
prob_gbm_cat_bal <- predict(gbm_cat_balanced, num_test[,-1], type="prob")
classes<-gsub("_","",names(prob_gbm_cat))
names(prob_gbm_cat) <- paste0(classes, "_pred_GBMcat")
names(prob_gbm_cat_bal) <- paste0(classes, "_pred_GBMcatBal")
observed <- gsub("_","",num_test$DC1_or_DC2)


prob_obs <- bind_cols(prob_gbm_cat, prob_gbm_cat_bal)

cat_pred<-str_split_fixed(colnames(prob_gbm_cat)[apply(prob_gbm_cat,1,which.max)], "_", 3)[,1]
catBal_pred<-str_split_fixed(colnames(prob_gbm_cat_bal)[apply(prob_gbm_cat_bal,1,which.max)], "_", 3)[,1]

prob_gbm_cat$BothDC1DC2_true <- ifelse(cat_pred == observed & cat_pred == "BothDC1DC2", 1, 0)
prob_gbm_cat$DC1only_true <- ifelse(cat_pred == observed & cat_pred == "DC1only", 1, 0)
prob_gbm_cat$DC2only_true <- ifelse(cat_pred == observed & cat_pred == "DC2only", 1, 0)

prob_gbm_cat_bal$BothDC1DC2_true <- ifelse(catBal_pred == observed & catBal_pred == "BothDC1DC2", 1, 0)
prob_gbm_cat_bal$DC1only_true <- ifelse(catBal_pred == observed & catBal_pred == "DC1only", 1, 0)
prob_gbm_cat_bal$DC2only_true <- ifelse(catBal_pred == observed & catBal_pred == "DC2only", 1, 0)

#Trying multiROC
roc_gbm_cat <- multi_roc(prob_gbm_cat)
roc_gbm_catBal <- multi_roc(prob_gbm_cat_bal)

#trying plot_roc_data
roc_gbm_cat_df <- plot_roc_data(roc_gbm_cat)
roc_gbm_catBal_df <- plot_roc_data(roc_gbm_catBal)

#Taking the micro average
cat_roc_df<-roc_gbm_cat_df[roc_gbm_cat_df$Group == "Micro",]   #AUC=0.9486734
catBal_roc_df<-roc_gbm_catBal_df[roc_gbm_catBal_df$Group == "Micro",]   #AUC=0.9592003

cat_roc_df$FPR <- 1-cat_roc_df$Specificity
cat_roc_df$model <- "GBM_Categorical"
catBal_roc_df$FPR <- 1-catBal_roc_df$Specificity
catBal_roc_df$model <- "GBM_Categorical_Bal"

pooled_sens_spec <- rbind(cat_roc_df,catBal_roc_df)

#Plot
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/roc_curve.pdf")
pooled_sens_spec %>%
  group_by(model) %>%
  ggplot(aes(x=FPR, y=Sensitivity))+
  geom_step(aes(color=model)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2))
dev.off()

