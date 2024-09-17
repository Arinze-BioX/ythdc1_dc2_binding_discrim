#BiocManager::install("GenomicRanges")

#load packages
library(GenomicRanges)
library(tidyverse)
library(pheatmap)

#read in metadata
metadata <- read.table("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/metadata.tsv",
 sep="\t", header = TRUE)
#I determined that an output type of IDR thresholded peaks, "conservative IDR thresholded peaks" or 
#"replicated peaks" covers oll the targets
metadata <- metadata[metadata$"Output.type" %in% c("IDR thresholded peaks",
"replicated peaks", "conservative IDR thresholded peaks") & 
metadata$File.analysis.title != "ENCODE3 GRCh38" &
metadata$"Audit.ERROR" == "",]

#Take note of replicates for each target
write.csv(table(metadata$"Experiment.target", metadata$"Biological.replicate.s."), 
"./data/replicates_per_target.csv")

#for each experiment accession, pick the file accession with the max size 
fileID<-character()
cnt=0
for (experiment in unique(metadata$"Experiment.accession")){
    max_size <- max(metadata[metadata$"Experiment.accession" == experiment, names(metadata) == "Size"])
    new_fileID <- metadata[metadata$"Experiment.accession" == experiment & 
    metadata$Size == max_size, names(metadata) %in% c("File.accession")]
    fileID <- append(fileID, new_fileID)
}
metadata <- metadata[metadata$"File.accession" %in% fileID, names(metadata) %in% c("File.accession",
"Experiment.target","Biological.replicate.s.", "Size", "Experiment.accession", "Output.type")]

#write final metadata file
write.csv(metadata, "./data/final_metadata.csv")


#load selected files
files<-paste0("./data/raw/", metadata$File.accession, ".bed")
filenames<-gsub("-human", "", paste0(metadata$Experiment.target, "_", metadata$File.accession ))
gene_names<-filenames
bed_list <- list()
for (i in 1:length(files)){
    bed_list[[i]] <- read.table(files[i], head=FALSE, sep="\t")
    names(bed_list[[i]])<-c("chrom", "start", "end", "strand", 
    "score_notImp", "x", "pVal_log10", "q_val", "y", "z")
    bed_list[[i]]<-bed_list[[i]][,c(1:3,7)]
}
names(bed_list) <- filenames

#Get DC2 and DC1 peaks
DC1_peaks<- read.table("/datacommons/ydiaolab/sun/DC1DC2peaks/DC1_peaks.bed", header=FALSE, sep="\t")
DC2_peaks<- read.table("/datacommons/ydiaolab/sun/DC1DC2peaks/DC2_peaks.narrowPeak", 
header=FALSE, sep="\t")[,c(1,2,3,7,8)]


#convert bedfiles to Genomic Ranges objects
gr_encode<-list()
i=0
names_pred<-character()
for(bed in bed_list){
    i=i+1
    gr_encode[[i]] <- makeGRangesFromDataFrame(bed, keep.extra.columns=TRUE, ignore.strand=TRUE)
    names_pred <- append(names_pred, names(bed_list)[i])
}
names(gr_encode)<-names_pred
names(DC2_peaks)<-c("chrom", "start", "end", "score", "pVal_log10")
names(DC1_peaks)<-c("chrom", "start", "end")
dc1_range <- makeGRangesFromDataFrame(DC1_peaks, keep.extra.columns=TRUE, ignore.strand=TRUE)
dc2_range <- makeGRangesFromDataFrame(DC2_peaks, keep.extra.columns=TRUE, ignore.strand=TRUE)


#Get Union of DC1 and DC2
dc1_dc2_union <- GenomicRanges::union(dc1_range,dc2_range, ignore.strand=TRUE)

#construct dc1_dc2 variable
dc1Only_vec<- dc1_dc2_union %over% dc1_range[dc1_range %outside% dc2_range,]
dc2Only_vec<- dc1_dc2_union %over% dc2_range[dc2_range %outside% dc1_range,]
dc1_dc2_vec <- rep("Both_DC1_DC2", length(dc1_dc2_union))
dc1_dc2_vec[dc1Only_vec]<- "DC1_only"
dc1_dc2_vec[dc2Only_vec]<- "DC2_only"
dc1_dc2_vec <- factor(dc1_dc2_vec, levels=c("DC1_only", "DC2_only", "Both_DC1_DC2"))

#Make PeakIDs
peakID<-paste0("Peak_", 1:length(dc1_dc2_union), "_", dc1_dc2_vec)
dc1_dc2_union$"peakID"<-peakID

#Make full categorical dataframe
categorical_df <- data.frame(peakID=peakID, dc1_dc2_vec=dc1_dc2_vec)
i=3
for(gr in gr_encode){
    gr_vec <- ifelse(dc1_dc2_union %over% gr, 1, 0)
    categorical_df[,i] <- gr_vec
    i=i+1
}
names(categorical_df) <- c("peakID", "DC1_or_DC2", names(gr_encode))

#save categorical_df
write.csv(categorical_df, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/final_data_categorical.csv")



#Make full numeric dataframe
numerical_df <- data.frame(peakID=peakID, dc1_dc2_vec=dc1_dc2_vec)
i=3
for(gr in gr_encode){
    gr_vec <- ifelse(dc1_dc2_union %over% gr, gr$pVal_log10, 0)
    numerical_df[,i] <- gr_vec
    i=i+1
}
names(numerical_df) <- c("peakID", "DC1_or_DC2", names(gr_encode))

#save categorical_df
write.csv(numerical_df, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/final_data_numerical.csv")



#Plot correlation of predictors
cat_cor<- cor(categorical_df[,-c(1,2)], method="spearman")
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/categorical_cor_spearman.pdf")
pheatmap(cat_cor, cluster_rows = FALSE, scale = "none", cluster_cols = FALSE)
dev.off()
write.csv(cat_cor, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/categorical_cor_mat.csv")


num_cor<- cor(numerical_df[,-c(1,2)], method="spearman")
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/numerical_cor_spearman.pdf", width=12, height=12)
pheatmap(num_cor, cluster_rows = FALSE, scale = "none", cluster_cols = FALSE)
dev.off()
write.csv(num_cor, "/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/numerical_cor_mat.csv")

num_cor<- cor(numerical_df[,-c(1,2)], method="pearson")
pdf("/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/results/numerical_cor_pearson.pdf", width=12, height=12)
pheatmap(num_cor, cluster_rows = FALSE, scale = "none", cluster_cols = FALSE)
dev.off()

#save union Granges
write.csv(as.data.frame(dc1_dc2_union), 
"/datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/data/Final_union_granges.csv")
