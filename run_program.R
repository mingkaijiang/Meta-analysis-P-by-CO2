##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

#### clear wk space
rm(list=ls(all=TRUE))

##### Master script
### source
source("prepare.R")

#### check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V1.csv",strip.white=T)

myDF <- as.data.frame(myDF)

### recalculate all the mean effect size
myDF <- make_mean_effect_size_recalculation(inDF=myDF)

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
### return a dataframe that exclude extremely high P addition experiment
subDF <- make_basic_summary_stats_plots(myDF)

### Make plots - biomass
make_biomass_plot(inDF=subDF) 


### To do list:
### 1. Finish basic plot with manuscript quality figures
### 2. Follow example in Baig to plot individual additive effect size for each study

### Comment:
### need to find a way to constrain P addition range
### right now eP can be 1000 times bigger than aP. 
### Check with stat book how to weigh the studies



