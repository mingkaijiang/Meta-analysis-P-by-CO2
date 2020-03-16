assign_model_stats_and_forest_plot_advanced <- function(tDF, intDF, res, var.name) {
    
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
                                       0.2,  #HP/LP = 5,
                                       0.25, #HP/LP = 4
                                       0.5), #HP/LP = 2
                      addx=T) 
    
    #predDF <- predict(res, newmods = c(log(1-0), 
    #                                   log(1-0.01), #HP/LP = 100
    #                                   log(1-0.02), #HP/LP = 50
    #                                   log(1-0.05), #HP/LP = 20
    #                                   log(1-0.1),  #HP/LP = 10
    #                                   log(1-0.25), #HP/LP = 4
    #                                   log(1-0.5)), #HP/LP = 2
    #                  addx=T) 
    
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
    
    #intDF$effect_size_0[intDF$variable==var.name] <- predDF$pred[1]
    #intDF$ci_lb_0[intDF$variable==var.name] <- predDF$ci.lb[1]
    #intDF$ci_ub_0[intDF$variable==var.name] <- predDF$ci.ub[1]
    #
    #intDF$effect_size_0.01[intDF$variable==var.name] <- predDF$pred[2]
    #intDF$ci_lb_0.01[intDF$variable==var.name] <- predDF$ci.lb[2]
    #intDF$ci_ub_0.01[intDF$variable==var.name] <- predDF$ci.ub[2]
    #
    #intDF$effect_size_0.02[intDF$variable==var.name] <- predDF$pred[3]
    #intDF$ci_lb_0.02[intDF$variable==var.name] <- predDF$ci.lb[3]
    #intDF$ci_ub_0.02[intDF$variable==var.name] <- predDF$ci.ub[3]
    #
    #intDF$effect_size_0.05[intDF$variable==var.name] <- predDF$pred[4]
    #intDF$ci_lb_0.05[intDF$variable==var.name] <- predDF$ci.lb[4]
    #intDF$ci_ub_0.05[intDF$variable==var.name] <- predDF$ci.ub[4]
    #
    #intDF$effect_size_0.1[intDF$variable==var.name] <- predDF$pred[5]
    #intDF$ci_lb_0.1[intDF$variable==var.name] <- predDF$ci.lb[5]
    #intDF$ci_ub_0.1[intDF$variable==var.name] <- predDF$ci.ub[5]
    #
    #intDF$effect_size_0.2[intDF$variable==var.name] <- predDF$pred[6]
    #intDF$ci_lb_0.2[intDF$variable==var.name] <- predDF$ci.lb[6]
    #intDF$ci_ub_0.2[intDF$variable==var.name] <- predDF$ci.ub[6]
    #
    #intDF$effect_size_0.25[intDF$variable==var.name] <- predDF$pred[7]
    #intDF$ci_lb_0.25[intDF$variable==var.name] <- predDF$ci.lb[7]
    #intDF$ci_ub_0.25[intDF$variable==var.name] <- predDF$ci.ub[7]
    #
    #intDF$effect_size_0.5[intDF$variable==var.name] <- predDF$pred[8]
    #intDF$ci_lb_0.5[intDF$variable==var.name] <- predDF$ci.lb[8]
    #intDF$ci_ub_0.5[intDF$variable==var.name] <- predDF$ci.ub[8]
    
    
    predDF2 <- predict(res, newmods = c(0.2)) 
    
    ### forest plot
    pdf(paste0("output/step2/interaction_", var.name, ".pdf"),
        height=16, width=9)
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
    addpoly(predDF2$pred, sei = predDF2$se, 
            mlab = c("HP/LP = 5"), cex=0.6)
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
    text(-13.5, -3.5, paste0("ne = ", l), cex = 0.6)
    text(-13.5, -2.5, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    return(intDF)
    
}