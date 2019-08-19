assign_CO2_effect_model_stats_and_forest_plot_advanced <- function(tDF, sumDF, res, var.name, trt) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$b[1]
    sumDF$se[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$se[1]
    sumDF$p_value[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$pval[1]
    sumDF$ns[sumDF$variable==var.name&sumDF$P_treatment==trt] <- ns
    sumDF$ne[sumDF$variable==var.name&sumDF$P_treatment==trt] <- l
    sumDF$ci_lb[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$ci.lb[1]
    sumDF$ci_ub[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$ci.ub[1]
    
    sumDF$P_moderator[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$b[2]
    sumDF$mod_p_value[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$pval[2]
    sumDF$mod_ci_lb[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$ci.lb[2]
    sumDF$mod_ci_ub[sumDF$variable==var.name&sumDF$P_treatment==trt] <- res$ci.ub[2]
    
    
    return(sumDF)
    
    
}