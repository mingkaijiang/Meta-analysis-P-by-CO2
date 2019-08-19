prepare_summary_co2_effect_nonwoody_df_advanced <- function() {
    sumDF <- data.frame(c(1:34), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("id", "variable", "P_treatment", "CO2_effect",
                         "ns", "ne", "se", "p_value", "ci_lb", "ci_ub",
                         "P_moderator", "mod_p_value", "mod_ci_lb", "mod_ci_ub")

    
    sumDF$variable <- rep(c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass",
                            "aboveground_biomass", "belowground_biomass",
                            "leaf_N_content", "leaf_P_content", 
                            "leaf_N_concentration", "root_N_concentration", 
                            "leaf_P_concentration", "root_P_concentration", 
                            "CO2_assimilation_rate", "leaf_area", "SLA", "LMA",
                            "Root_length"),each=2)
    
    sumDF$P_treatment <- rep(c("eP", "aP"), 17)
    
    return(sumDF)
    
}