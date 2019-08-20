assign_model_stats_and_forest_plot_basic <- function(tDF, intDF, res, var.name) {
    
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
    
    ### forest plot
    pdf(paste0("output/statistics_interaction/", var.name, ".pdf"),
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-14, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(tDF$Vegetation_type),
                        as.character(tDF$Species),
                        as.character(tDF$Mycorrhizae_2), 
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
                                                   "Species",
                                                   "Mycorrhizal",
                                                   expression(paste(eCO[2], "/", aCO[2])),
                                                   "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
         c("type","", "association", "", "", "duration"), cex=0.7)
    text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-13.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-13.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    return(intDF)
    
    
}