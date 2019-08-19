##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

#### clear wk space
rm(list=ls(all=TRUE))

################################# Master script ################################# 
############## source
source("prepare.R")


############## check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V5_low_vs_high_P.csv",strip.white=T)

myDF <- as.data.frame(myDF)


### recalculate all the mean effect size
myDF <- make_mean_effect_size_recalculation(inDF=myDF)

### remove P treatment of zero low P addition
#myDF <- subset(myDF, Trt_aP > 0.0)

### make consistent standard error confidence intervals
myDF <- make_consistent_confidence_interval(inDF=myDF, return.option="all_se")

### check ratio of CO2 treatment
myDF$Trt_eC_by_aC <- myDF$Trt_eCO2/myDF$Trt_aCO2

### check P treatment
myDF$Trt_eP_by_aP <- myDF$Trt_eP / myDF$Trt_aP

### check P reduction ratio
#myDF$Trt_P_reduction <- (myDF$Trt_eP - myDF$Trt_aP) / myDF$Trt_eP

############## Exclude some extremely high P addition experiment
subDF100 <- subset(myDF, Trt_aCO2 < 410)

#### generate species list
generate_species_list()

############## Basic statistics that summarize 
### number of studies
### number of data entries
### vegetation type
### CO2 and P treatment
### etc.
make_basic_summary_stats_plots()


############## Statistical analysis - metafor
### reprogressing the dataset to calculate individual means and variance for the interaction term
#reDF <- reprocessing_interaction_term(inDF=subDF)
reDF100 <- reprocessing_interaction_term(inDF=subDF100)
reDF100$random_factor <- as.numeric(reDF100$Literature)

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

### calculate percent response
intDF$int_pct <- (exp(intDF$interaction) - 1) * 100
intDF$se_pct <- (exp(intDF$se) - 1) * 100
intDF$ci_lb_pct <- (exp(intDF$ci_lb) - 1) * 100
intDF$ci_ub_pct <- (exp(intDF$ci_ub) - 1) * 100


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
#sumDF2 <- metafor_p_statistics_nutrient_ratio_100_aCO2_V5(reDF100, sumDF2)



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
#sumDF2 <- metafor_p_statistics_nutrient_ratio_100_eCO2(reDF100, sumDF2)


### calculate percent response
sumDF2$P_effect_pct <- (exp(sumDF2$P_effect) - 1) * 100
sumDF2$se_pct <- (exp(sumDF2$se) - 1) * 100
sumDF2$ci_lb_pct <- (exp(sumDF2$ci_lb) - 1) * 100
sumDF2$ci_ub_pct <- (exp(sumDF2$ci_ub) - 1) * 100

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

### calculate percent response
sumDF$CO2_effect_pct <- (exp(sumDF$CO2_effect) - 1) * 100
sumDF$se_pct <- (exp(sumDF$se) - 1) * 100
sumDF$ci_lb_pct <- (exp(sumDF$ci_lb) - 1) * 100
sumDF$ci_ub_pct <- (exp(sumDF$ci_ub) - 1) * 100

write.csv(sumDF, "output/metafor_summary_plot/co2_effect_all.csv", row.names=F)
write.csv(sumDF2, "output/metafor_summary_plot/p_effect_all.csv", row.names=F)
write.csv(intDF, "output/metafor_summary_plot/lp_effect_on_co2_response_all.csv", row.names=F)

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
make_split_interaction_effect_chart_6(sumDF, sumDF2, intDF)


##############
#### subset woody plant DF
wdDF <- subset(reDF100, Vegetation_type=="Woody")

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


### calculate percent response
sumDF2$CO2_effect_pct <- (exp(sumDF2$CO2_effect) - 1) * 100
sumDF2$se_pct <- (exp(sumDF2$se) - 1) * 100
sumDF2$ci_lb_pct <- (exp(sumDF2$ci_lb) - 1) * 100
sumDF2$ci_ub_pct <- (exp(sumDF2$ci_ub) - 1) * 100

intDF2$int_pct <- (exp(intDF2$interaction) - 1) * 100
intDF2$se_pct <- (exp(intDF2$se) - 1) * 100
intDF2$ci_lb_pct <- (exp(intDF2$ci_lb) - 1) * 100
intDF2$ci_ub_pct <- (exp(intDF2$ci_ub) - 1) * 100

##############
#### subset woody plant DF
nwdDF <- subset(reDF100, Vegetation_type=="Nonwoody")

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


### calculate percent response
sumDF3$CO2_effect_pct <- (exp(sumDF3$CO2_effect) - 1) * 100
sumDF3$se_pct <- (exp(sumDF3$se) - 1) * 100
sumDF3$ci_lb_pct <- (exp(sumDF3$ci_lb) - 1) * 100
sumDF3$ci_ub_pct <- (exp(sumDF3$ci_ub) - 1) * 100

intDF3$int_pct <- (exp(intDF3$interaction) - 1) * 100
intDF3$se_pct <- (exp(intDF3$se) - 1) * 100
intDF3$ci_lb_pct <- (exp(intDF3$ci_lb) - 1) * 100
intDF3$ci_ub_pct <- (exp(intDF3$ci_ub) - 1) * 100




#### plot woody and nonwoody comparison
#plot_woody_nonwoody_comparison(intDF2, intDF3, sumDF2, sumDF3)
#plot_woody_nonwoody_comparison_2(intDF2, intDF3, sumDF2, sumDF3)
plot_woody_nonwoody_comparison_3(intDF2, intDF3, sumDF2, sumDF3)


#### statistics comparing woody and non-woody plants
test_between_group_heterogeneity(reDF100)
#compute_statistics_for_woody_and_nonwoody_comparison(wdDF, nwdDF)

#### mycorrhizal associations
test_woody_plants_mycorrhzial_effects(wdDF)


#### plot a leaf N vs. leaf P concentration comparison
plot_leaf_N_P_concentration_comparison()



############## Compare mycorrhizae groups
test_plant_mycorrhizal_effect(reDF100)



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