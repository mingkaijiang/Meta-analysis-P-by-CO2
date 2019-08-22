assign_model_stats_advanced <- function(tDF, intDF, res, var.name) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$ns[intDF$variable==var.name] <- ns
    intDF$ne[intDF$variable==var.name] <- l
    
    
    ### predicted
    predDF <- predict(res, newmods = c(0, 
                                       0.01, #HP/LP = 100
                                       0.02, #HP/LP = 50
                                       0.05, #HP/LP = 20
                                       0.1,  #HP/LP = 10
                                       0.2,
                                       0.25, #HP/LP = 4
                                       0.5), #HP/LP = 2
                      addx=T) 

    intDF$interaction[intDF$variable==var.name] <- predDF$pred[6]
    intDF$se[intDF$variable==var.name] <- predDF$se[6]
    intDF$ci_lb[intDF$variable==var.name] <- predDF$ci.lb[6]
    intDF$ci_ub[intDF$variable==var.name] <- predDF$ci.ub[6]
    
    
    #intDF$interaction[intDF$variable==var.name] <- res$b[1]
    #intDF$se[intDF$variable==var.name] <- res$se[1]
    #intDF$ci_lb[intDF$variable==var.name] <- res$ci.lb[1]
    #intDF$ci_ub[intDF$variable==var.name] <- res$ci.ub[1]
    #intDF$p_value[intDF$variable==var.name] <- res$pval[1]
    
    intDF$P_moderator[intDF$variable==var.name] <- res$b[2]
    intDF$mod_p_value[intDF$variable==var.name] <- res$pval[2]
    intDF$mod_ci_lb[intDF$variable==var.name] <- res$ci.lb[2]
    intDF$mod_ci_ub[intDF$variable==var.name] <- res$ci.ub[2]
    

    
    return(intDF)
    
}