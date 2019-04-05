prepare_summary_interaction_effect_df <- function() {
    
    id.list <- seq(1.5, 61.5, by=2)
    sumDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("id", "variable", "interaction", 
                         "ns", "ne", "se", "p_value")

    
    sumDF$variable <- c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass",
                            "leaf_N_content", "stem_N_content", "root_N_content", "total_N_content",
                            "leaf_P_content", "stem_P_content", "root_P_content", "total_P_content",
                            "leaf_N_concentration", "stem_N_concentration", "root_N_concentration", "total_N_concentration",
                            "leaf_P_concentration", "stem_P_concentration", "root_P_concentration", "total_P_concentration",
                            "CO2_assimilation_rate", "stomatal_conductance", "leaf_area", "SLA", "LMA",
                            "Root_length", "N_uptake", "P_uptake", "WUE", "NUE", "PUE")
    
    return(sumDF)
    
}