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

subDF100 <- subset(myDF, Trt_eP_by_aP <= 100)


############## Basic statistics that summarize 
### number of studies
### number of data entries
### vegetation type
### CO2 and P treatment
### etc.
make_basic_summary_stats_plots()

############### make overall plots
#### Make plots - biomass
#make_biomass_plots(inDF=subDF) 
#
#### Make plots - concentration
#make_concentration_plots(inDF=subDF)
#
#### Make plots - nutrient ratio 
#make_nutrient_ratio_plots(inDF=subDF)
#
#### Make plots - morphology
#make_morphology_plots(inDF=subDF)
#
#### Make plots - nutrient uptake
#make_nutrient_uptake_plots(inDF=subDF)
#
#### Make plots - resource use efficiency
#make_resource_use_efficiency_plots(inDF=subDF)
#
#### Make plots - gas exchange
#make_gas_exchange_plots(inDF=subDF)
#
############### Make response ratio plots per study
#### Make plots - biomass
#make_biomass_plots_per_study(inDF=subDF) 
#
#### Make plots - concentration
#make_concentration_plots_per_study(inDF=subDF)
#
#### Make plots - nutrient ratio 
#make_nutrient_ratio_plots_per_study(inDF=subDF)
#
#### Make plots - morphology
#make_morphology_plots_per_study(inDF=subDF)
#
#### Make plots - nutrient uptake
#make_nutrient_uptake_plots_per_study(inDF=subDF)
#
#### Make plots - resource use efficiency
#make_resource_use_efficiency_plots_per_study(inDF=subDF)
#
#### Make plots - gas exchange
#make_gas_exchange_plots_per_study(inDF=subDF)
#
############### make response ratio plots along concentration gradients
#### Make plots - biomass
#make_biomass_plots_along_gradients(inDF=subDF) 
#
#### Make plots - concentration
#make_concentration_plots_along_gradients(inDF=subDF)
#
#### Make plots - nutrient ratio 
#make_nutrient_ratio_plots_along_gradients(inDF=subDF)
#
#### Make plots - morphology
#make_morphology_plots_along_gradients(inDF=subDF)
#
#### Make plots - nutrient uptake
#make_nutrient_uptake_plots_along_gradients(inDF=subDF)
#
#### Make plots - resource use efficiency
#make_resource_use_efficiency_plots_along_gradients(inDF=subDF)
#
#### Make plots - gas exchange
#make_gas_exchange_plots_along_gradients(inDF=subDF)


############## Statistical analysis - metafor
### reprogressing the dataset to calculate individual means and variance for the interaction term
reDF <- reprocessing_interaction_term(inDF=subDF)
reDF100 <- reprocessing_interaction_term(inDF=subDF100)
### split the dataset into individual response variable
### and perform statistical analysis for the overall effect size and variance
### also check for data issues, make plots

### Biomass
metafor_statistics_biomass(reDF)

### concentration
metafor_statistics_concentration(reDF)

### morphology
metafor_statistics_morphology(reDF)

### nutrient uptake
metafor_statistics_nutrient_uptake(reDF)

### resource use efficiency
metafor_statistics_resource_use_efficiency(reDF)

### gas exchange
metafor_statistics_gas_exchange(reDF)



##############
######## Interaction effect
### Biomass
metafor_statistics_biomass_100(reDF100)

### concentration
metafor_statistics_concentration_100(reDF100)

### morphology
metafor_statistics_morphology_100(reDF100)

### nutrient uptake
metafor_statistics_nutrient_uptake_100(reDF100)

### resource use efficiency
metafor_statistics_resource_use_efficiency_100(reDF100)

### gas exchange
metafor_statistics_gas_exchange_100(reDF100)


##############
####### P effect
reDF100 <- reprocessing_p_effect_term(inDF=reDF100)

### Biomass
metafor_p_statistics_biomass_100(reDF100)

### concentration
metafor_p_statistics_concentration_100(reDF100)

### morphology
metafor_p_statistics_morphology_100(reDF100)

### nutrient uptake
metafor_p_statistics_nutrient_uptake_100(reDF100)

### resource use efficiency
metafor_p_statistics_resource_use_efficiency_100(reDF100)

### gas exchange
metafor_p_statistics_gas_exchange_100(reDF100)

##############
######## CO2 effect
reDF100 <- reprocessing_co2_effect_term(inDF=reDF100)


### Biomass
metafor_co2_statistics_biomass_100(reDF100)

### concentration
metafor_co2_statistics_concentration_100(reDF100)

### morphology
metafor_co2_statistics_morphology_100(reDF100)

### nutrient uptake
metafor_co2_statistics_nutrient_uptake_100(reDF100)

### resource use efficiency
metafor_co2_statistics_resource_use_efficiency_100(reDF100)

### gas exchange
metafor_co2_statistics_gas_exchange_100(reDF100)


############## scenario illustration
scenario_illustration_plot()

scenario_illustration_plot_high_P()

############## make overall plots
### Make plots - biomass
make_biomass_plots_100(inDF=subDF100) 

### Make plots - concentration
make_concentration_plots_100(inDF=subDF100)

### Make plots - morphology
make_morphology_plots_100(inDF=subDF100)

### Make plots - nutrient uptake
make_nutrient_uptake_plots_100(inDF=subDF100)

### Make plots - resource use efficiency
make_resource_use_efficiency_plots_100(inDF=subDF100)

### Make plots - gas exchange
make_gas_exchange_plots_100(inDF=subDF100)


############## make gradient subplot 10
#
##### ep/ap < 10
#make_subplots_along_gradients(inDF=subDF)
#
#### ep/ap < 100
#make_subplots_along_gradients_100(inDF=subDF100)



