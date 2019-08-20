assign_model_stats_advanced <- function(tDF, intDF, res, var.name) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable==var.name] <- res$b[1]
    intDF$se[intDF$variable==var.name] <- res$se[1]
    intDF$p_value[intDF$variable==var.name] <- res$pval[1]
    intDF$ns[intDF$variable==var.name] <- ns
    intDF$ne[intDF$variable==var.name] <- l
    intDF$ci_lb[intDF$variable==var.name] <- res$ci.lb[1]
    intDF$ci_ub[intDF$variable==var.name] <- res$ci.ub[1]
    
    intDF$P_moderator[intDF$variable==var.name] <- res$b[2]
    intDF$mod_p_value[intDF$variable==var.name] <- res$pval[2]
    intDF$mod_ci_lb[intDF$variable==var.name] <- res$ci.lb[2]
    intDF$mod_ci_ub[intDF$variable==var.name] <- res$ci.ub[2]
    

    
    return(intDF)
    
}