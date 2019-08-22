assign_P_effect_model_stats_and_forest_plot_advanced <- function(tDF, sumDF2, res, var.name, trt) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$ns[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- ns
    sumDF2$ne[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- l
    
    ### predicted
    predDF <- predict(res, newmods = c(0, 
                                       0.01, #HP/LP = 100
                                       0.02, #HP/LP = 50
                                       0.05, #HP/LP = 20
                                       0.1,  #HP/LP = 10
                                       0.2,  #HP/LP = 5,
                                       0.25, #HP/LP = 4
                                       0.5), #HP/LP = 2
                      addx=T) 
    
    #sumDF2$P_effect[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$b[1]
    #sumDF2$se[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$se[1]
    #sumDF2$p_value[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$pval[1]
    #sumDF2$ci_lb[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.lb[1]
    #sumDF2$ci_ub[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.ub[1]
    
    sumDF2$P_effect[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- predDF$pred[6]
    sumDF2$se[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- predDF$se[6]
    sumDF2$ci_lb[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- predDF$ci.lb[6]
    sumDF2$ci_ub[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- predDF$ci.ub[6]
    
    sumDF2$P_moderator[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$b[2]
    sumDF2$mod_p_value[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$pval[2]
    sumDF2$mod_ci_lb[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.lb[2]
    sumDF2$mod_ci_ub[sumDF2$variable==var.name&sumDF2$CO2_treatment==trt] <- res$ci.ub[2]
    
    
    return(sumDF2)
    
    
}