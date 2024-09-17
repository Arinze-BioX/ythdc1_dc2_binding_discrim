#load required packaes 
library(mltools)
library(gbm)
library(dplyr)
library(data.table)
library(caret)
library(pheatmap)
library(VennDiagram)

#Plot importance for each class
#read in original df
df <- read.csv("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/final_data_categorical.csv")[,-c(1,2)]
labs <- df$DC1_or_DC2
df <- df %>% mutate_all(~replace(., . == 0, -1))


#one-hot encoding
new_df <- as.data.table(df)
new_df$DC1_or_DC2 <- as.factor(labs)
new_df <- mltools::one_hot(new_df, col = 'DC1_or_DC2')


#DC1_or_DC2_DC2_only, DC1_or_DC2_DC1_only, DC1_or_DC2_Both_DC1_DC2

#get correlation
cor_mat_DC1Only <- cor(df[,-1], new_df$DC1_or_DC2_DC1_only)
cor_mat_DC2Only <- cor(df[,-1], new_df$DC1_or_DC2_DC2_only)
cor_mat_Both <- cor(df[,-1], new_df$DC1_or_DC2_Both_DC1_DC2)

write.csv()

#Read in models
gbm_cat_bal <- readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat_balanced.rds")
gbm_cat <- readRDS("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/models/gbm_cat.rds")

#get importance for each model
imp_bal <- varImp(gbm_cat_bal)$importance
imp_nobal <- varImp(gbm_cat)$importance

#Plot importance alone
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/gbm_cat_bal_importance.pdf", height=15)
plot(varImp(gbm_cat_bal), top=105)
dev.off()

pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/gbm_cat_Unbalanced_importance.pdf", height=15)
plot(varImp(gbm_cat), top=105)
dev.off()

#plot importance * correlation
bal_df<-data.frame(DC1Only=imp_bal$Overall*scale(cor_mat_DC1Only[,1]), DC2Only=imp_bal$Overall*scale(cor_mat_DC2Only[,1]), 
Both=imp_bal$Overall*scale(cor_mat_Both[,1]), row.names=(rownames(imp_bal)))
cor_df<-data.frame(DC1Only=cor_mat_DC1Only[,1], DC2Only=cor_mat_DC2Only[,1], 
Both=cor_mat_Both[,1], row.names=(rownames(imp_bal)))

write.csv(bal_df, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/Importance_times_correlation_balanced.csv")
write.csv(cor_df, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/correlation.csv")

pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/gbm_cat_balanced_importance_times_corr_heatmap.pdf", height=12)
pheatmap(bal_df, cluster_rows=FALSE, cluster_cols =FALSE, scale = "column")
dev.off()


#draw venn diagram
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/DC1_DC2_overlap.pdf")
draw.pairwise.venn(area1=(11619+5687), area2=(11619+32938),cross.area=11619,
                   category=c("YTHDC1","YTHDC2"),fill=c("#A6D6F6","#F6C6A6"))
dev.off()