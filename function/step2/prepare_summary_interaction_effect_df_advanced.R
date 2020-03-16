prepare_summary_interaction_effect_df_advanced <- function() {
    
    id.list <- seq(1.5, 71.5, by=2)
    sumDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)#,
                        #NA, NA, NA, NA, NA, NA, NA,
                        #NA, NA, NA, NA, NA, NA, NA,
                        #NA, NA, NA, NA, NA, NA, NA,
                        #NA, NA, NA)
    
    colnames(sumDF) <- c("id", "variable", "interaction", 
                         "ns", "ne", "se", "p_value", "ci_lb", "ci_ub",
                         "P_moderator", "mod_p_value", "mod_ci_lb", "mod_ci_ub")#,
                         #"effect_size_0", "ci_lb_0", "ci_ub_0",
                         #"effect_size_0.01", "ci_lb_0.01", "ci_ub_0.01",
                         #"effect_size_0.02", "ci_lb_0.02", "ci_ub_0.02",
                         #"effect_size_0.05", "ci_lb_0.05", "ci_ub_0.05",
                         #"effect_size_0.1", "ci_lb_0.1", "ci_ub_0.1",
                         #"effect_size_0.2", "ci_lb_0.2", "ci_ub_0.2",
                         #"effect_size_0.25", "ci_lb_0.25", "ci_ub_0.25",
                         #"effect_size_0.5", "ci_lb_0.5", "ci_ub_0.5")

    
    sumDF$variable <- c("leaf_biomass", "stem_biomass", "aboveground_biomass","root_biomass", "total_biomass", 
                            "leaf_N_content", "stem_N_content", "root_N_content", "total_N_content",
                            "leaf_P_content", "stem_P_content", "root_P_content", "total_P_content",
                            "leaf_N_concentration", "stem_N_concentration", "root_N_concentration", "total_N_concentration",
                            "leaf_P_concentration", "stem_P_concentration", "root_P_concentration", "total_P_concentration",
                            "CO2_assimilation_rate", "stomatal_conductance", "leaf_area", "SLA", "LMA",
                            "Root_length", "N_uptake", "P_uptake", "WUE", "NUE", "PUE",
                        "leaf_NP", "stem_NP", "root_NP", "total_NP")
    
    return(sumDF)
    
}