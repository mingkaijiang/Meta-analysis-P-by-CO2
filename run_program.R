##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

#### clear wk space
rm(list=ls(all=TRUE))

################################# Master script ################################# 
############## source
source("prepare.R")

############## check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V1.csv",strip.white=T)

myDF <- as.data.frame(myDF)

### recalculate all the mean effect size
myDF <- make_mean_effect_size_recalculation(inDF=myDF)

### remove P treatment of zero low P addition
myDF <- subset(myDF, Trt_aP > 0.0)

### make consistent standard error confidence intervals
myDF <- make_consistent_confidence_interval(inDF=myDF, return.option="all_se")

### check ratio of CO2 treatment
myDF$Trt_eC_by_aC <- myDF$Trt_eCO2/myDF$Trt_aCO2

### check P treatment
myDF$Trt_eP_by_aP <- myDF$Trt_eP / myDF$Trt_aP

### check P reduction ratio
#myDF$Trt_P_reduction <- (myDF$Trt_eP - myDF$Trt_aP) / myDF$Trt_eP

############## Exclude some extremely high P addition experiment
subDF <- subset(myDF, Trt_eP_by_aP <= 10)

############## Basic statistics that summarize 
### number of studies
### number of data entries
### species numbers
### vegetation type
### CO2 and P treatment
### etc.
make_basic_summary_stats_plots(test=subDF)


############## make overall plots
### Make plots - biomass
make_biomass_plots(inDF=subDF) 

### Make plots - concentration
make_concentration_plots(inDF=subDF)

### Make plots - nutrient ratio 
make_nutrient_ratio_plots(inDF=subDF)

### Make plots - morphology
make_morphology_plots(inDF=subDF)

### Make plots - nutrient uptake
make_nutrient_uptake_plots(inDF=subDF)

### Make plots - resource use efficiency
make_resource_use_efficiency_plots(inDF=subDF)

### Make plots - gas exchange
make_gas_exchange_plots(inDF=subDF)

############## Make response ratio plots per study
### Make plots - biomass
make_biomass_plots_per_study(inDF=subDF) 

### Make plots - concentration
make_concentration_plots_per_study(inDF=subDF)

### Make plots - nutrient ratio 
make_nutrient_ratio_plots_per_study(inDF=subDF)

### Make plots - morphology
make_morphology_plots_per_study(inDF=subDF)

### Make plots - nutrient uptake
make_nutrient_uptake_plots_per_study(inDF=subDF)

### Make plots - resource use efficiency
make_resource_use_efficiency_plots_per_study(inDF=subDF)

### Make plots - gas exchange
make_gas_exchange_plots_per_study(inDF=subDF)


### To do list:
### 1. Finish basic plot with manuscript quality figures
### 2. Add individual plots, following example of Baig, and subject to data availability
### 3. Perform statistical analysis to compute CIs



