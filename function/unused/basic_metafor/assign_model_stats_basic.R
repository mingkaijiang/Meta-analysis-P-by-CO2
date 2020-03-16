assign_model_stats_basic <- function(tDF, intDF, res, var.name) {
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable==var.name] <- res$b[1]
    intDF$se[intDF$variable==var.name] <- res$se
    intDF$p_value[intDF$variable==var.name] <- res$pval
    intDF$ns[intDF$variable==var.name] <- ns
    intDF$ne[intDF$variable==var.name] <- l
    intDF$ci_lb[intDF$variable==var.name] <- res$ci.lb
    intDF$ci_ub[intDF$variable==var.name] <- res$ci.ub
    

    
    return(intDF)
    
    
}