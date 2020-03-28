#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

if(!dir.exists("output/step1")) {
    dir.create("output/step1", showWarnings = FALSE)
}

if(!dir.exists("output/step2")) {
    dir.create("output/step2", showWarnings = FALSE)
}

if(!dir.exists("output/step2/supplementary")) {
    dir.create("output/step2/supplementary", showWarnings = FALSE)
}

if(!dir.exists("output/step3")) {
    dir.create("output/step3", showWarnings = FALSE)
}

if(!dir.exists("output/step4")) {
    dir.create("output/step4", showWarnings = FALSE)
}

if(!dir.exists("output/step5")) {
    dir.create("output/step5", showWarnings = FALSE)
}

if(!dir.exists("output/step5/supplementary")) {
    dir.create("output/step5/supplementary", showWarnings = FALSE)
}


#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               grid,
               cowplot,
               metafor,
               mgcv,
               weights,
               meta,
               igraph,
               tidyverse,
               network)  

#### Sourcing all R files in the modules subdirectory
#source_basic_scripts <- dir("function/basic_processing", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z1 in source_basic_scripts)source(z1)
#
#source_basic_metafor_scripts <- dir("function/basic_metafor", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z2 in source_basic_metafor_scripts)source(z2)
#
#source_conc_gradient_scripts <- dir("function/concentration_gradient_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z3 in source_conc_gradient_scripts)source(z3)
#
#source_metafor_co2_scripts <- dir("function/metafor_co2_effect_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z4 in source_metafor_co2_scripts)source(z4)
#
#source_metafor_p_scripts <- dir("function/metafor_p_effect_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z5 in source_metafor_p_scripts)source(z5)
#
#source_overall_analysis_scripts <- dir("function/overall_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z6 in source_overall_analysis_scripts)source(z6)
#
#source_per_study_scripts <- dir("function/per_study_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z7 in source_per_study_scripts)source(z7)
#
#
#source_per_study_scripts <- dir("function/by_plant_group", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z8 in source_per_study_scripts)source(z8)
#
#source_heterogeneity_scripts <- dir("function/between_group_heterogeneity", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
#for(z9 in source_heterogeneity_scripts)source(z9)

source_step1 <- dir("function/step1", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in source_step1)source(z1)

source_step2 <- dir("function/step2", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z2 in source_step2)source(z2)

source_step3 <- dir("function/step3", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z3 in source_step3)source(z3)

source_step4 <- dir("function/step4", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z4 in source_step4)source(z4)

source_step5 <- dir("function/step5", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z5 in source_step5)source(z5)



