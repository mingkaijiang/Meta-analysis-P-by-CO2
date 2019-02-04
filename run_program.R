##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

##### Master script
### source
source("prepare.R")

#### check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V1.csv",strip.white=T)

myDF <- as.data.frame(myDF)

### make numbers numeric
myDF$Interaction_additive_aCaP <- as.numeric(as.character(myDF$Interaction_additive_aCaP))
myDF$Interaction_multiplicative_aCaP <- as.numeric(as.character(myDF$Interaction_multiplicative_aCaP))

myDF$Interaction_additive_aCaP_aCeP <- as.numeric(as.character(myDF$Interaction_additive_aCeP))
myDF$Interaction_multiplicative_aCeP <- as.numeric(as.character(myDF$Interaction_multiplicative_aCeP))

myDF$Sample.Size <- as.numeric(as.character(myDF$Sample.Size))

### remove P treatment of zero low P addition
myDF <- subset(myDF, Trt_aP > 0.0)

### make consistent standard error confidence intervals
myDF <- make_consistent_confidence_interval(inDF=myDF, return.option="all_se")


### Basic statistics that summarize 
### number of studies
### number of data entries
### species numbers
### vegetation type
### CO2 and P treatment
### etc. 
make_basic_summary_stats_plots(myDF)


### To do list:
### 1. Finish basic plot with manuscript quality figures
### 2. Follow example in Baig to plot individual additive effect size for each study

### Comment:
### need to find a way to constrain P addition range
### right now eP can be 1000 times bigger than aP. 
### Check with stat book how to weigh the studies



