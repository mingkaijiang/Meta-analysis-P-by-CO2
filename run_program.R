##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

#### clear wk space
rm(list=ls(all=TRUE))

################################# Master script ################################# 
############## source
source("prepare.R")


############## check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V6_HP_control.csv",strip.white=T)

myDF <- as.data.frame(myDF)


### recalculate all the mean effect size
myDF <- make_mean_effect_size_recalculation(inDF=myDF)

### make consistent standard error confidence intervals
myDF <- make_consistent_confidence_interval(inDF=myDF, return.option="all_se")

### check ratio of CO2 treatment
myDF$Trt_eC_by_aC <- myDF$Trt_eCO2/myDF$Trt_aCO2

### check P treatment
myDF$Trt_eP_by_aP <- myDF$Trt_eP / myDF$Trt_aP

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
#make_basic_summary_stats_plots()


############## Statistical analysis - metafor
### reprogressing the dataset to calculate individual means and variance for the interaction term
reDF100 <- reprocessing_interaction_term(inDF=subDF100)
reDF100$random_factor <- as.factor(reDF100$Literature)
reDF100$Trt_LP_HP <- reDF100$Trt_aP/reDF100$Trt_eP

mean(reDF100$Trt_LP_HP)
median(reDF100$Trt_LP_HP)

##### a detailed summary table 
generate_a_detailed_summary_table(reDF100)


##############
######## Interaction effect

#intDF <- prepare_summary_interaction_effect_df()
intDF <- prepare_summary_interaction_effect_df_advanced()

intDF <- metafor_statistics_advanced(reDF100, intDF)
#intDF <- metafor_statistics_basic(reDF100, intDF)


### calculate percent response
intDF$int_pct <- (exp(intDF$interaction) - 1) * 100
intDF$se_pct <- (exp(intDF$se) - 1) * 100
intDF$ci_lb_pct <- (exp(intDF$ci_lb) - 1) * 100
intDF$ci_ub_pct <- (exp(intDF$ci_ub) - 1) * 100
write.csv(intDF, "output/metafor_summary_plot/lp_effect_on_co2_response_all.csv", row.names=F)


##############
####### P effect under aCO2 and eCO2
reDF100 <- reprocessing_p_effect_term(inDF=reDF100)

### prepare a storage dataframe for all summary information
### useful for making later summary plot
sumDF2 <- prepare_summary_p_effect_df_advanced()

####### P effect under aCO2 
sumDF2 <- metafor_p_statistics_aCO2_advanced(reDF100, sumDF2)
#sumDF2 <- metafor_p_statistics_aCO2_basic(reDF100, sumDF2)


####### P effect under eCO2 
sumDF2 <- metafor_p_statistics_eCO2_advanced(reDF100, sumDF2)
#sumDF2 <- metafor_p_statistics_eCO2_basic(reDF100, sumDF2)

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
sumDF <- prepare_summary_co2_effect_df_advanced()


######## CO2 effect under eP
#sumDF <- metafor_co2_statistics_eP_advanced(reDF100, sumDF)
sumDF <- metafor_co2_statistics_eP_basic(reDF100, sumDF)


######## CO2 effect under aP
#sumDF <- metafor_co2_statistics_aP_advanced(reDF100, sumDF)
sumDF <- metafor_co2_statistics_aP_basic(reDF100, sumDF)


### calculate percent response
sumDF$CO2_effect_pct <- (exp(sumDF$CO2_effect) - 1) * 100
sumDF$se_pct <- (exp(sumDF$se) - 1) * 100
sumDF$ci_lb_pct <- (exp(sumDF$ci_lb) - 1) * 100
sumDF$ci_ub_pct <- (exp(sumDF$ci_ub) - 1) * 100

write.csv(sumDF, "output/metafor_summary_plot/co2_effect_all.csv", row.names=F)
write.csv(sumDF2, "output/metafor_summary_plot/p_effect_all.csv", row.names=F)

write.csv(reDF100, "output/metafor_summary_plot/reprocessed_input_data.csv", row.names=F)




#### this is the plot script used for main text
make_split_interaction_effect_chart_6(sumDF, sumDF2, intDF)


##############
#### subset woody plant DF
wdDF <- subset(reDF100, Vegetation_type=="Woody")

#### Interaction effect for woody plants
intDF.wd <- prepare_summary_interaction_effect_woody_df_advanced()

#### metafor statistics
intDF.wd <- metafor_statistics_woody_plants_100(wdDF, intDF.wd)

#### CO2 effect under aP and eP for woody plants
sumDF.wd <- prepare_summary_co2_effect_woody_df()

#### CO2 effect under eP
sumDF.wd <- metafor_co2_statistics_woody_plants_100_eP(wdDF, sumDF.wd)

#### CO2 effect under aP
sumDF.wd <- metafor_co2_statistics_woody_plants_100_aP(wdDF, sumDF.wd)


### calculate percent response
sumDF.wd$CO2_effect_pct <- (exp(sumDF.wd$CO2_effect) - 1) * 100
sumDF.wd$se_pct <- (exp(sumDF.wd$se) - 1) * 100
sumDF.wd$ci_lb_pct <- (exp(sumDF.wd$ci_lb) - 1) * 100
sumDF.wd$ci_ub_pct <- (exp(sumDF.wd$ci_ub) - 1) * 100

intDF.wd$int_pct <- (exp(intDF.wd$interaction) - 1) * 100
intDF.wd$se_pct <- (exp(intDF.wd$se) - 1) * 100
intDF.wd$ci_lb_pct <- (exp(intDF.wd$ci_lb) - 1) * 100
intDF.wd$ci_ub_pct <- (exp(intDF.wd$ci_ub) - 1) * 100

##############
#### subset woody plant DF
nwdDF <- subset(reDF100, Vegetation_type=="Nonwoody")

#### Interaction effect for nonwoody plants
intDF.nwd <- prepare_summary_interaction_effect_woody_df()

#### metafor statistics
intDF.nwd <- metafor_statistics_nonwoody_plants_100(nwdDF, intDF.nwd)

#### CO2 effect under aP and eP for nonwoody plants
sumDF.nwd <- prepare_summary_co2_effect_nonwoody_df()

#### CO2 effect under eP
sumDF.nwd <- metafor_co2_statistics_nonwoody_plants_100_eP(nwdDF, sumDF.nwd)

#### CO2 effect under aP
sumDF.nwd <- metafor_co2_statistics_nonwoody_plants_100_aP(nwdDF, sumDF.nwd)


### calculate percent response
sumDF.nwd$CO2_effect_pct <- (exp(sumDF.nwd$CO2_effect) - 1) * 100
sumDF.nwd$se_pct <- (exp(sumDF.nwd$se) - 1) * 100
sumDF.nwd$ci_lb_pct <- (exp(sumDF.nwd$ci_lb) - 1) * 100
sumDF.nwd$ci_ub_pct <- (exp(sumDF.nwd$ci_ub) - 1) * 100

intDF.nwd$int_pct <- (exp(intDF.nwd$interaction) - 1) * 100
intDF.nwd$se_pct <- (exp(intDF.nwd$se) - 1) * 100
intDF.nwd$ci_lb_pct <- (exp(intDF.nwd$ci_lb) - 1) * 100
intDF.nwd$ci_ub_pct <- (exp(intDF.nwd$ci_ub) - 1) * 100




#### plot woody and nonwoody comparison
#plot_woody_nonwoody_comparison(intDF2, intDF3, sumDF2, sumDF3)
#plot_woody_nonwoody_comparison_2(intDF2, intDF3, sumDF2, sumDF3)
plot_woody_nonwoody_comparison_4(intDF.wd, intDF.nwd, sumDF.wd, sumDF.nwd)





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