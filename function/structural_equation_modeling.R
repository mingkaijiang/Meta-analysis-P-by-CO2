structural_equation_modeling_test <- function(reDF) {
    
    ### library
    require(lavaan)
    require(tidyr)
    
    ### clean the data
    sDF <- reDF[,c("Literature", "Category", "Variable", "Trt_aCO2", "Trt_eCO2",
                   "Trt_aP", "Trt_eP", "Sample.Size",
                   "Species", "Vegetation_type", "Soil_weight_in_pot",
                   "Pot_volume", "Experiment_duration", "Trt_eC_by_aC",
                   "Trt_eP_by_aP", "interaction", "log_interaction", "v_variance")]
    
    sDF$Variable <- gsub(" ", "_", sDF$Variable)
    
    testDF <- summaryBy(log_interaction~Variable+Vegetation_type, data=sDF, FUN=mean, keep.names=T)
    
    wdDF <- spread(data = testDF, 
                 key = "Variable",
                 value = "log_interaction")
    
    
    ### structural equation
    HS.model <- 'Biomass =~ Leaf_biomass + Root_biomass + Stem_biomass
                 Concentration =~ Leaf_N_concentration + Root_N_concentration 
                 Morphology =~ SLA + Total_root_length'
    
    fit <- cfa(HS.model, data = wdDF)
    
    
}