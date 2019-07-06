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

subDF100 <- subset(myDF, Trt_eP_by_aP <= 100 & Trt_aCO2 <= 410)

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



############## make overall plots
#### Make plots - biomass
#make_biomass_plots_100(inDF=subDF100) 
#
#### Make plots - concentration
#make_concentration_plots_100(inDF=subDF100)
#
#### Make plots - morphology
#make_morphology_plots_100(inDF=subDF100)
#
#### Make plots - nutrient uptake
#make_nutrient_uptake_plots_100(inDF=subDF100)
#
#### Make plots - resource use efficiency
#make_resource_use_efficiency_plots_100(inDF=subDF100)
#
#### Make plots - gas exchange
#make_gas_exchange_plots_100(inDF=subDF100)


############## make gradient subplot 10
#
##### ep/ap < 10
#make_subplots_along_gradients(inDF=subDF)
#
#### ep/ap < 100
#make_subplots_along_gradients_100(inDF=subDF100)




############## Statistical analysis - metafor
### reprogressing the dataset to calculate individual means and variance for the interaction term
#reDF <- reprocessing_interaction_term(inDF=subDF)
reDF100 <- reprocessing_interaction_term(inDF=subDF100)
### split the dataset into individual response variable
### and perform statistical analysis for the overall effect size and variance
### also check for data issues, make plots

#### Biomass
#metafor_statistics_biomass(reDF)
#
#### concentration
#metafor_statistics_concentration(reDF)
#
#### morphology
#metafor_statistics_morphology(reDF)
#
#### nutrient uptake
#metafor_statistics_nutrient_uptake(reDF)
#
#### resource use efficiency
#metafor_statistics_resource_use_efficiency(reDF)
#
#### gas exchange
#metafor_statistics_gas_exchange(reDF)


##### a detailed summary table 
generate_a_detailed_summary_table(reDF100)



##############
######## Interaction effect

intDF <- prepare_summary_interaction_effect_df()


### Biomass
intDF <- metafor_statistics_biomass_100(reDF100, intDF)

### concentration
intDF <- metafor_statistics_concentration_100(reDF100, intDF)

### morphology
intDF <- metafor_statistics_morphology_100(reDF100, intDF)

### nutrient uptake
intDF <- metafor_statistics_nutrient_uptake_100(reDF100, intDF)

### resource use efficiency
intDF <- metafor_statistics_resource_use_efficiency_100(reDF100, intDF)

### gas exchange
intDF <- metafor_statistics_gas_exchange_100(reDF100, intDF)


### nutrient ratio
intDF <- metafor_statistics_nutrient_ratio_100(reDF100, intDF)




##############
####### P effect under aCO2 and eCO2
reDF100 <- reprocessing_p_effect_term(inDF=reDF100)

### prepare a storage dataframe for all summary information
### useful for making later summary plot
sumDF2 <- prepare_summary_p_effect_df()

####### P effect under aCO2 
### Biomass
sumDF2 <- metafor_p_statistics_biomass_100_aCO2(reDF100, sumDF2)

### concentration
sumDF2 <- metafor_p_statistics_concentration_100_aCO2(reDF100, sumDF2)

### morphology
sumDF2 <- metafor_p_statistics_morphology_100_aCO2(reDF100, sumDF2)

### nutrient uptake
sumDF2 <- metafor_p_statistics_nutrient_uptake_100_aCO2(reDF100, sumDF2)

### resource use efficiency
sumDF2 <- metafor_p_statistics_resource_use_efficiency_100_aCO2(reDF100, sumDF2)

### gas exchange
sumDF2 <- metafor_p_statistics_gas_exchange_100_aCO2(reDF100, sumDF2)

### nutrient ratio
sumDF2 <- metafor_p_statistics_nutrient_ratio_100_aCO2(reDF100, sumDF2)



####### P effect under eCO2 
### Biomass
sumDF2 <- metafor_p_statistics_biomass_100_eCO2(reDF100, sumDF2)

### concentration
sumDF2 <- metafor_p_statistics_concentration_100_eCO2(reDF100, sumDF2)

### morphology
sumDF2 <- metafor_p_statistics_morphology_100_eCO2(reDF100, sumDF2)

### nutrient uptake
sumDF2 <- metafor_p_statistics_nutrient_uptake_100_eCO2(reDF100, sumDF2)

### resource use efficiency
sumDF2 <- metafor_p_statistics_resource_use_efficiency_100_eCO2(reDF100, sumDF2)

### gas exchange
sumDF2 <- metafor_p_statistics_gas_exchange_100_eCO2(reDF100, sumDF2)

### nutrient ratio
sumDF2 <- metafor_p_statistics_nutrient_ratio_100_eCO2(reDF100, sumDF2)


##############
######## CO2 effect under aP and eP
reDF100 <- reprocessing_co2_effect_term(inDF=reDF100)

### prepare a storage dataframe for all summary information
### useful for making later summary plot
sumDF <- prepare_summary_co2_effect_df()


######## CO2 effect under eP
### Biomass
sumDF <- metafor_co2_statistics_biomass_100_eP(reDF100, sumDF)

### concentration
sumDF <- metafor_co2_statistics_concentration_100_eP(reDF100, sumDF)

### morphology
sumDF <- metafor_co2_statistics_morphology_100_eP(reDF100, sumDF)

### nutrient uptake
sumDF <- metafor_co2_statistics_nutrient_uptake_100_eP(reDF100, sumDF)

### resource use efficiency
sumDF <- metafor_co2_statistics_resource_use_efficiency_100_eP(reDF100, sumDF)

### gas exchange
sumDF <- metafor_co2_statistics_gas_exchange_100_eP(reDF100, sumDF)

### nutrient ratio
sumDF <- metafor_co2_statistics_nutrient_ratio_100_eP(reDF100, sumDF)



######## CO2 effect under aP
### Biomass
sumDF <- metafor_co2_statistics_biomass_100_aP(reDF100, sumDF)

### concentration
sumDF <- metafor_co2_statistics_concentration_100_aP(reDF100, sumDF)

### morphology
sumDF <- metafor_co2_statistics_morphology_100_aP(reDF100, sumDF)

### nutrient uptake
sumDF <- metafor_co2_statistics_nutrient_uptake_100_aP(reDF100, sumDF)

### resource use efficiency
sumDF <- metafor_co2_statistics_resource_use_efficiency_100_aP(reDF100, sumDF)

### gas exchange
sumDF <- metafor_co2_statistics_gas_exchange_100_aP(reDF100, sumDF)

### nutrient ratio
sumDF <- metafor_co2_statistics_nutrient_ratio_100_aP(reDF100, sumDF)




############## plot all significant responses
#plot_significant_response_ratio_100(sumDF)



############## make eCO2 at low P and high P plot
#make_eCO2_effect_at_lowP_highP_chart(sumDF)

############## make lP effect at aCO2 and eCO2
#make_lP_effect_at_aCO2_eCO2_chart(sumDF2)

############## make interaction effect 
#make_interaction_effect_chart(sumDF, sumDF2, intDF)


#### this is the plot script used for main text
#make_split_interaction_effect_chart(sumDF, sumDF2, intDF)
#make_split_interaction_effect_chart_2(sumDF, sumDF2, intDF)
#make_split_interaction_effect_chart_3(sumDF, sumDF2, intDF)
#make_split_interaction_effect_chart_4(sumDF, sumDF2, intDF)
make_split_interaction_effect_chart_5(sumDF, sumDF2, intDF)




##############
#### subset woody plant DF
wdDF <- subset(reDF100, Vegetation_type=="Tree")

#### Interaction effect for woody plants
intDF2 <- prepare_summary_interaction_effect_woody_df()

#### metafor statistics
intDF2 <- metafor_statistics_woody_plants_100(wdDF, intDF2)

#### CO2 effect under aP and eP for woody plants
sumDF2 <- prepare_summary_co2_effect_woody_df()

#### CO2 effect under eP
sumDF2 <- metafor_co2_statistics_woody_plants_100_eP(wdDF, sumDF2)

#### CO2 effect under aP
sumDF2 <- metafor_co2_statistics_woody_plants_100_aP(wdDF, sumDF2)


##############
#### subset woody plant DF
nwdDF <- subset(reDF100, Vegetation_type!="Tree")

#### Interaction effect for nonwoody plants
intDF3 <- prepare_summary_interaction_effect_woody_df()

#### metafor statistics
intDF3 <- metafor_statistics_nonwoody_plants_100(nwdDF, intDF3)

#### CO2 effect under aP and eP for nonwoody plants
sumDF3 <- prepare_summary_co2_effect_nonwoody_df()

#### CO2 effect under eP
sumDF3 <- metafor_co2_statistics_nonwoody_plants_100_eP(nwdDF, sumDF3)

#### CO2 effect under aP
sumDF3 <- metafor_co2_statistics_nonwoody_plants_100_aP(nwdDF, sumDF3)


#### plot woody and nonwoody comparison
plot_woody_nonwoody_comparison(intDF2, intDF3, sumDF2, sumDF3)







#### make predictions
#metafor_statistics_gam_model(reDF100, intDF)
#metafor_statistics_mixed_effect_model(reDF100, intDF)



############## scenario illustration
scenario_illustration_plot()

scenario_illustration_plot_high_P()

scenario_illustration_simplified_plot_high_P()


### To do plan
### gradient example using leaf biomass
### mixed model prediction, split data variables, with gradient of P addition