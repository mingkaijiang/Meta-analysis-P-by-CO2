#################################################################################
##### Meta-analysis of P x CO2 interaction
##### Author: Mingkai Jiang
##### Code structure
##### 0. Prepare
##### 1. Clean data and make basic summary tables and plots
##### 2. make metafor plots and tables based on all data
##### 3. make metafor plots and tables comparing woody and nonwoody responses
##### 4. check mycorrhizal effect


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

#### Simplifying the original data
myDF <- reduce_duplicated_time_data_entries(inDF=myDF)


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
#make_basic_summary_stats_plots(inDF=myDF)

#### plot a leaf N vs. leaf P concentration comparison
plot_leaf_N_P_concentration_comparison(inDF=myDF)

### plot author cluster plot
plot_paper_clusters()

### add a second layer of random factor
#myDF <- add_second_random_factor(inDF=myDF)


#################################################################################
##### Step 2. make metafor plots and tables

### prepare summary data frames
intDF <- prepare_summary_interaction_effect_df_advanced()
sumDF2 <- prepare_summary_p_effect_df_advanced()
sumDF <- prepare_summary_co2_effect_df_advanced()


#### re-analyze interaction effect
intDF <- make_step2_metafor_statistics_advanced(inDF=myDF, intDF=intDF)

### calculate percent response
intDF$int_pct <- (exp(intDF$interaction) - 1) * 100
intDF$se_pct <- (exp(intDF$se) - 1) * 100
intDF$ci_lb_pct <- (exp(intDF$ci_lb) - 1) * 100
intDF$ci_ub_pct <- (exp(intDF$ci_ub) - 1) * 100
write.csv(intDF, "output/step2/interaction_responses_simplified.csv", row.names=F)



### P effect under aCO2 
sumDF2 <- make_step2_metafor_p_statistics_aCO2_advanced(inDF=myDF, sumDF2=sumDF2)

### P effect under eCO2 
sumDF2 <- make_step2_metafor_p_statistics_eCO2_advanced(inDF=myDF, sumDF2=sumDF2)

### calculate percent response
sumDF2$P_effect_pct <- (exp(sumDF2$P_effect) - 1) * 100
sumDF2$se_pct <- (exp(sumDF2$se) - 1) * 100
sumDF2$ci_lb_pct <- (exp(sumDF2$ci_lb) - 1) * 100
sumDF2$ci_ub_pct <- (exp(sumDF2$ci_ub) - 1) * 100
write.csv(sumDF2, "output/step2/p_effect_simplified.csv", row.names=F)


### CO2 effect under eP
sumDF <- make_step2_metafor_co2_statistics_eP_basic(inDF=myDF, sumDF=sumDF)

### CO2 effect under aP
sumDF <- make_step2_metafor_co2_statistics_aP_basic(inDF=myDF, sumDF=sumDF)

### calculate percent response
sumDF$CO2_effect_pct <- (exp(sumDF$CO2_effect) - 1) * 100
sumDF$se_pct <- (exp(sumDF$se) - 1) * 100
sumDF$ci_lb_pct <- (exp(sumDF$ci_lb) - 1) * 100
sumDF$ci_ub_pct <- (exp(sumDF$ci_ub) - 1) * 100
write.csv(sumDF, "output/step2/co2_effect_simplified.csv", row.names=F)


### make summary plots of the interaction and individual responses 
### go into function to plot!!!
#make_step2_summary_chart(sumDF=sumDF, sumDF2=sumDF2, intDF=intDF)
#make_graphic_abstract_chart(sumDF=sumDF, sumDF2=sumDF2, intDF=intDF)



#################################################################################
##### Step 3. check woody and non-woody comparison
### subset woody and nonwoody plant DF
wdDF <- subset(myDF, Vegetation_type=="Woody")
nwdDF <- subset(myDF, Vegetation_type=="Nonwoody")

### prepare storage dfs
## Interaction effect for woody and non-woody plants
intDF.wd <- prepare_step3_summary_interaction_effect_woody_df_advanced()
intDF.nwd <- prepare_step3_summary_interaction_effect_nonwoody_df()

## CO2 effect under aP and eP for woody plants
sumDF.wd <- prepare_step3_summary_co2_effect_woody_df()
sumDF.nwd <- prepare_step3_summary_co2_effect_nonwoody_df()

## metafor interaction 
intDF.wd <- make_step3_metafor_statistics_woody_plants(inDF=wdDF, intDF=intDF.wd)
intDF.nwd <- make_step3_metafor_statistics_nonwoody_plants(inDF=nwdDF, intDF=intDF.nwd)


### metafor CO2 effect under eP
sumDF.wd <- make_step3_metafor_co2_statistics_woody_plants_eP(inDF=wdDF, sumDF=sumDF.wd)
sumDF.nwd <- make_step3_metafor_co2_statistics_nonwoody_plants_eP(inDF=nwdDF, sumDF=sumDF.nwd)

### metafor CO2 effect under aP
sumDF.wd <- make_step3_metafor_co2_statistics_woody_plants_aP(inDF=wdDF, sumDF=sumDF.wd)
sumDF.nwd <- make_step3_metafor_co2_statistics_nonwoody_plants_aP(inDF=nwdDF, sumDF=sumDF.nwd)

### calculate percent response - woody
sumDF.wd$CO2_effect_pct <- (exp(sumDF.wd$CO2_effect) - 1) * 100
sumDF.wd$se_pct <- (exp(sumDF.wd$se) - 1) * 100
sumDF.wd$ci_lb_pct <- (exp(sumDF.wd$ci_lb) - 1) * 100
sumDF.wd$ci_ub_pct <- (exp(sumDF.wd$ci_ub) - 1) * 100

intDF.wd$int_pct <- (exp(intDF.wd$interaction) - 1) * 100
intDF.wd$se_pct <- (exp(intDF.wd$se) - 1) * 100
intDF.wd$ci_lb_pct <- (exp(intDF.wd$ci_lb) - 1) * 100
intDF.wd$ci_ub_pct <- (exp(intDF.wd$ci_ub) - 1) * 100

### calculate percent response - nonwoody
sumDF.nwd$CO2_effect_pct <- (exp(sumDF.nwd$CO2_effect) - 1) * 100
sumDF.nwd$se_pct <- (exp(sumDF.nwd$se) - 1) * 100
sumDF.nwd$ci_lb_pct <- (exp(sumDF.nwd$ci_lb) - 1) * 100
sumDF.nwd$ci_ub_pct <- (exp(sumDF.nwd$ci_ub) - 1) * 100

intDF.nwd$int_pct <- (exp(intDF.nwd$interaction) - 1) * 100
intDF.nwd$se_pct <- (exp(intDF.nwd$se) - 1) * 100
intDF.nwd$ci_lb_pct <- (exp(intDF.nwd$ci_lb) - 1) * 100
intDF.nwd$ci_ub_pct <- (exp(intDF.nwd$ci_ub) - 1) * 100

### statistics comparing woody and non-woody plants
test_step3_between_group_heterogeneity(inDF=myDF)

### plot woody and nonwoody comparison
### go into function to plot!!!
#make_step3_woody_nonwoody_comparison_chart(intDF.wd, intDF.nwd, sumDF.wd, sumDF.nwd)


#################################################################################
##### Step 4. Mycorrhizal comparison
#### statistics comparing ECM and AM plants
mycoDF <- myDF[myDF$Mycorrhizae_2 %in%c("ECM", "AM"),]
test_step4_between_group_heterogeneity_mycorrhizae(inDF=mycoDF)

#### statistics comparing woody vs nonwoody of AM plants
test_step4_between_group_heterogeneity_mycorrhizae_woody_nonwoody(subDF=mycoDF)

### plot mycorrhizal comparison
### go into function to plot!!!
#make_step4_mycorrhizal_comparison_chart(inDF=mycoDF)

#################################################################################
#### The end
#################################################################################