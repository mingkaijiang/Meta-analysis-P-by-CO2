#################################################################################
##### Meta-analysis of P x CO2 interaction
##### Author: Mingkai Jiang
##### Code structure
##### 0. Prepare
##### 1. Clean data
##### 2. 



#################################################################################
###### Step 0: prepare the necessary code and packages
### clear wk space
rm(list=ls(all=TRUE))

### source
source("prepare.R")


#################################################################################
###### Step 1: read in input file and prepare it
### read input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V7_HP_control.csv",strip.white=T)


### recalculate all the mean effect size
### make consistent standard error confidence intervals
### check ratio of CO2 and P treatment
### exclude data not needed
myDF <- make_step1_basic_processing(inDF=myDF)

### reprogressing the dataset to calculate individual means and variance for the interaction term
### re-categorize mycorrhizal type information
### print LP/HP ratios
myDF <- make_step1_reprocessing_interaction_term(inDF=myDF)

####### P effect under aCO2 and eCO2
myDF <- make_step1_reprocessing_p_effect_term(inDF=myDF)

######## CO2 effect under aP and eP
myDF <- make_step1_reprocessing_co2_effect_term(inDF=myDF)

### save reprcessed data
write.csv(myDF, "output/step2/input_data_reprocessed.csv", row.names=F)

### generate species list
### come back point: to make count statistics!!!
generate_species_list(inDF=myDF)

##### a detailed summary table 
generate_a_detailed_summary_table(inDF=myDF)


### Basic statistics that summarize 
### number of studies
### number of data entries
### vegetation type
### CO2 and P treatment
### need to go into function and plot
### come back point: to make nicer plots!!!!
make_basic_summary_stats_plots(inDF=myDF)


#################################################################################
##### Step 2. make metafor plots and tables

### prepare summary data frames
intDF <- prepare_summary_interaction_effect_df_advanced()
sumDF2 <- prepare_summary_p_effect_df_advanced()
sumDF <- prepare_summary_co2_effect_df_advanced()


### Interaction effect
intDF <- make_step2_metafor_statistics_advanced(inDF=myDF, intDF=intDF)

### calculate percent response
intDF$int_pct <- (exp(intDF$interaction) - 1) * 100
intDF$se_pct <- (exp(intDF$se) - 1) * 100
intDF$ci_lb_pct <- (exp(intDF$ci_lb) - 1) * 100
intDF$ci_ub_pct <- (exp(intDF$ci_ub) - 1) * 100
write.csv(intDF, "output/step2/interaction_responses_all.csv", row.names=F)



### P effect under aCO2 
sumDF2 <- make_step2_metafor_p_statistics_aCO2_advanced(inDF=myDF, sumDF2=sumDF2)

### P effect under eCO2 
sumDF2 <- make_step2_metafor_p_statistics_eCO2_advanced(inDF=myDF, sumDF2=sumDF2)

### calculate percent response
sumDF2$P_effect_pct <- (exp(sumDF2$P_effect) - 1) * 100
sumDF2$se_pct <- (exp(sumDF2$se) - 1) * 100
sumDF2$ci_lb_pct <- (exp(sumDF2$ci_lb) - 1) * 100
sumDF2$ci_ub_pct <- (exp(sumDF2$ci_ub) - 1) * 100
write.csv(sumDF2, "output/step2/p_effect_all.csv", row.names=F)



### CO2 effect under eP
sumDF <- make_step2_metafor_co2_statistics_eP_basic(inDF=myDF, sumDF=sumDF)

### CO2 effect under aP
sumDF <- make_step2_metafor_co2_statistics_aP_basic(inDF=myDF, sumDF=sumDF)

### calculate percent response
sumDF$CO2_effect_pct <- (exp(sumDF$CO2_effect) - 1) * 100
sumDF$se_pct <- (exp(sumDF$se) - 1) * 100
sumDF$ci_lb_pct <- (exp(sumDF$ci_lb) - 1) * 100
sumDF$ci_ub_pct <- (exp(sumDF$ci_ub) - 1) * 100
write.csv(sumDF, "output/step2/co2_effect_all.csv", row.names=F)





#### this is the plot script used for main text
make_split_interaction_effect_chart(sumDF, sumDF2, intDF)


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


#### statistics comparing ECM and AM plants
reDF100 <- reDF100[reDF100$Mycorrhizae_2 %in%c("ECM", "AM"),]
test_between_group_heterogeneity_mycorrhizae(reDF100)

#### statistics comparing woody vs nonwoody of AM plants
test_between_group_heterogeneity_mycorrhizae_woody_nonwoody(reDF100)



#compute_statistics_for_woody_and_nonwoody_comparison(wdDF, nwdDF)


#### plot a leaf N vs. leaf P concentration comparison
plot_leaf_N_P_concentration_comparison()



#### The end