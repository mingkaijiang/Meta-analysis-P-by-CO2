assign_P_effect_model_stats_and_forest_plot_advanced <- function(tDF, sumDF2, res, var.name, trt) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$b[1]
    sumDF2$se[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$se[1]
    sumDF2$p_value[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$pval[1]
    sumDF2$ns[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- ns
    sumDF2$ne[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- l
    sumDF2$ci_lb[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.lb[1]
    sumDF2$ci_ub[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.ub[1]
    
    sumDF2$P_moderator[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$b[2]
    sumDF2$mod_p_value[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$pval[2]
    sumDF2$mod_ci_lb[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.lb[2]
    sumDF2$mod_ci_ub[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.ub[2]
    
    
    return(sumDF2)
    
    
}