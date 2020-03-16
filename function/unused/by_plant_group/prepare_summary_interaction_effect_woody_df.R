prepare_summary_interaction_effect_woody_df <- function() {
    
    id.list <- seq(1.5, 33.5, by=2)
    sumDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("id", "variable", "interaction", 
                         "ns", "ne", "se", "p_value", "ci_lb", "ci_ub")

    
    sumDF$variable <- c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass",
                        "aboveground_biomass", "belowground_biomass",
                            "leaf_N_content", "leaf_P_content", 
                            "leaf_N_concentration", "root_N_concentration",
                            "leaf_P_concentration", "root_P_concentration",
                            "CO2_assimilation_rate","leaf_area", "SLA", "LMA",
                            "Root_length")
    
    return(sumDF)
    
}