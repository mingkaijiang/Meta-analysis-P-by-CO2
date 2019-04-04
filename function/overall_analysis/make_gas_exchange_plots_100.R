make_gas_exchange_plots_100 <- function(inDF) {
    
    if(!dir.exists("output/overall_gas_exchange_100")) {
        dir.create("output/overall_gas_exchange_100", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Gas Exchange")
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("CO2 assimilation rate", 
                                          "Stomatal conductance"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    variable.list <- unique(bioDF1$Variable)
    
    ### sample size
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="CO2 assimilation rate"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="Stomatal conductance"])
    
    
    ### create a dataframe to contain long format data for each plot (3 dataframes)
    pDF1 <- data.frame(c(0.8, 1.2, 1.8, 2.2), NA, NA, NA, NA, NA)
    colnames(pDF1) <- c("brk", "variable", "CO2_trt", "value", "pos", "neg")
    pDF1$variable <- rep(c("CO2 assimilation rate", 
                           "Stomatal conductance"),each=2)
    pDF1$CO2_trt <- rep(c("aC", "eC"),2)
    pDF2 <- pDF1
    
    
    ### assign values
    for (i in variable.list) {
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                                "aCeP_over_aCaP"])
        #
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                                "aCeP_over_aCaP"]) + 
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "aCeP_over_aCaP_sd"])
        #
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                                "aCeP_over_aCaP"]) -
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "aCeP_over_aCaP_sd"])
        #
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                                "eCeP_over_eCaP"])
        #
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                              "eCeP_over_eCaP"]) + 
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "eCeP_over_eCaP_sd"])
        #
        #pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                              "eCeP_over_eCaP"]) -
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "eCeP_over_eCaP_sd"])
        
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                            "aCaP_over_aCeP"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "aCaP_over_aCeP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "aCaP_over_aCeP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "aCaP_over_aCeP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "aCaP_over_aCeP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                            "eCaP_over_eCeP"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCaP_over_eCeP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCaP_over_eCeP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCaP_over_eCeP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCaP_over_eCeP_sd"])
    }
    
    
    
    
    ### rename pDF2
    names(pDF2)[3] <- "P_trt"
    pDF2$P_trt <- gsub("aC", "eP", pDF2$P_trt)
    pDF2$P_trt <- gsub("eC", "aP", pDF2$P_trt)
    
    ### assign values
    for (i in variable.list) {
        pDF2[pDF2$variable==i&pDF2$P_trt=="aP", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCaP_over_aCaP"])
        
        pDF2[pDF2$variable==i&pDF2$P_trt=="aP", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "eCaP_over_aCaP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCaP_over_aCaP_sd"])
        
        pDF2[pDF2$variable==i&pDF2$P_trt=="aP", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "eCaP_over_aCaP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCaP_over_aCaP_sd"])
        
        pDF2[pDF2$variable==i&pDF2$P_trt=="eP", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCeP_over_aCeP"])
        
        pDF2[pDF2$variable==i&pDF2$P_trt=="eP", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "eCeP_over_aCeP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCeP_over_aCeP_sd"])
        
        pDF2[pDF2$variable==i&pDF2$P_trt=="eP", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "eCeP_over_aCeP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCeP_over_aCeP_sd"])
    }
    
    ### create a dataframe to contain long format data for each plot (3 dataframes)
    pDF3 <- data.frame(c(1:2), NA, NA, NA, NA)
    colnames(pDF3) <- c("brk", "variable", "value", "pos", "neg")
    pDF3$variable <- c("CO2 assimilation rate", 
                       "Stomatal conductance")
    
    ### assign values
    for (i in variable.list) {
        #pDF3[pDF3$variable==i, "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                              "Interaction_multiplicative_aCaP"])
        #
        #pDF3[pDF3$variable==i, "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                            "Interaction_multiplicative_aCaP"]) + 
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "Interaction_multiplicative_aCaP_sd"])
        #
        #pDF3[pDF3$variable==i, "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
        #                                                            "Interaction_multiplicative_aCaP"]) -
        #    log(bioDF1.sm[bioDF1.sm$Variable==i,
        #              "Interaction_multiplicative_aCaP_sd"])
        
        pDF3[pDF3$variable==i, "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                         "Interaction_multiplicative_aCeP"])
        
        pDF3[pDF3$variable==i, "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                       "Interaction_multiplicative_aCeP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "Interaction_multiplicative_aCeP_sd"])
        
        pDF3[pDF3$variable==i, "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                       "Interaction_multiplicative_aCeP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "Interaction_multiplicative_aCeP_sd"])
    }
    
    
    ### plotting
    p1 <- ggplot() +
        geom_point(data=pDF1, stat = "identity", 
                   aes(brk, value, col=CO2_trt),
                   size=5) +
        geom_errorbar(data=pDF1, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos, col=CO2_trt), 
                      width=0.2, size=1) +
        ylab("P reduction RR") +
        scale_x_continuous(name = "Gax exchange",
                           breaks=c(1,2),
                           labels=c("A", 
                                    expression(g[s])))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        geom_hline(yintercept=0, linetype=2)+
        ylim(-2.2,2)+
        scale_color_manual(name=expression(paste(CO[2], " treatment")),
                           breaks=c("aC", "eC"),
                           values=c("black","red"))
    
    p2 <- ggplot() +
        geom_point(data=pDF2, stat = "identity", 
                   aes(brk, value, col=P_trt),
                   size=5) +
        geom_errorbar(data=pDF2, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos, col=P_trt), 
                      width=0.2, size=1) +
        ylab(expression(paste(eCO[2], " RR"))) +
        scale_x_continuous(name = "Gax exchange",
                           breaks=c(1,2),
                           labels=c("A", 
                                    expression(g[s])))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        geom_hline(yintercept=0, linetype=2)+
        ylim(-2,2)+
        scale_color_manual(name=paste("P treatment"),
                           breaks=c("eP", "aP"),
                           values=c("black","red"),
                           labels=c("hP", "lP"))
    
    p3 <- ggplot() +
        geom_point(data=pDF3, stat = "identity", 
                   aes(brk, value),
                   size=5) +
        geom_errorbar(data=pDF3, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos), 
                      width=0.2, size=1) +
        ylab(expression(paste("Interaction RR"))) +
        scale_x_continuous(name = "Gax exchange",
                           breaks=c(1,2),
                           labels=c("A", 
                                    expression(g[s])))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        geom_hline(yintercept=0, linetype=2)+
        ylim(-3,3)+
        annotate("text", x=0.5, y=-2.5, label="n = ", size=5)+
        annotate("text", x=1, y=-2.5, label=n1, size=5)+
        annotate("text", x=2, y=-2.5, label=n2, size=5)
    
    ### summary histgram of treatments
    pdf("output/overall_gas_exchange_100/summary_gas_exchange_overall_plot.pdf", width=4, height=12)
    
    plot_grid(p1, p2, p3,
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    

 }