#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               grid,
               cowplot,
               metafor,
               mgcv)  

#### Sourcing all R files in the modules subdirectory
source_basic_scripts <- dir("function/basic_processing", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in source_basic_scripts)source(z1)

source_basic_metafor_scripts <- dir("function/basic_metafor", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z2 in source_basic_metafor_scripts)source(z2)

source_conc_gradient_scripts <- dir("function/concentration_gradient_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z3 in source_conc_gradient_scripts)source(z3)

source_metafor_co2_scripts <- dir("function/metafor_co2_effect_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z4 in source_metafor_co2_scripts)source(z4)

source_metafor_p_scripts <- dir("function/metafor_p_effect_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z5 in source_metafor_p_scripts)source(z5)

source_overall_analysis_scripts <- dir("function/overall_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z6 in source_overall_analysis_scripts)source(z6)

source_per_study_scripts <- dir("function/per_study_analysis", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z7 in source_per_study_scripts)source(z7)





