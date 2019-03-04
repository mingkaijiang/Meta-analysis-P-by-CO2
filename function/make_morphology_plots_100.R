make_morphology_plots_100 <- function(inDF) {
    
    if(!dir.exists("output/overall_morphology_100")) {
        dir.create("output/overall_morphology_100", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Morphology")
    
    ### change LAI to leaf area and combine it with Total leaf area
    bioDF[bioDF$Variable=="LAI","Variable"] <- "Leaf area"
    bioDF$Variable[bioDF$Variable=="Total leaf area"] <- "Leaf area"
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total root length", "SLA", 
                                          "LMA", "Leaf area"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    variable.list <- unique(bioDF1$Variable)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Total root length"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="SLA"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="LMA"])
    n4 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf area"])
    
    ### create a dataframe to contain long format data for each plot (3 dataframes)
    pDF1 <- data.frame(c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 
                         3.8, 4.2), NA, NA, NA, NA, NA)
    colnames(pDF1) <- c("brk", "variable", "CO2_trt", "value", "pos", "neg")
    pDF1$variable <- rep(c("Total root length", "SLA", 
                           "LMA", "Leaf area"),each=2)
    pDF1$CO2_trt <- rep(c("aC", "eC"),4)
    pDF2 <- pDF1
    
    
    ### assign values
    for (i in variable.list) {
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                            "aCeP_over_aCaP"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "aCeP_over_aCaP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "aCeP_over_aCaP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "aCeP_over_aCaP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "aCeP_over_aCaP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                            "eCeP_over_eCaP"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCeP_over_eCaP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCeP_over_eCaP_sd"])
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                                          "eCeP_over_eCaP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "eCeP_over_eCaP_sd"])
    }
    
    
    
    
    ### rename pDF2
    names(pDF2)[3] <- "P_trt"
    pDF2$P_trt <- gsub("aC", "aP", pDF2$P_trt)
    pDF2$P_trt <- gsub("eC", "eP", pDF2$P_trt)
    
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
    pDF3 <- data.frame(c(1:4), NA, NA, NA, NA)
    colnames(pDF3) <- c("brk", "variable", "value", "pos", "neg")
    pDF3$variable <- c("Total root length", "SLA", 
                       "LMA", "Leaf area")
    
    ### assign values
    for (i in variable.list) {
        pDF3[pDF3$variable==i, "value"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                         "Interaction_multiplicative_aCaP"])
        
        pDF3[pDF3$variable==i, "pos"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                       "Interaction_multiplicative_aCaP"]) + 
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "Interaction_multiplicative_aCaP_sd"])
        
        pDF3[pDF3$variable==i, "neg"] <- log(bioDF1.sm[bioDF1.sm$Variable==i,
                                                       "Interaction_multiplicative_aCaP"]) -
            log(bioDF1.sm[bioDF1.sm$Variable==i,
                          "Interaction_multiplicative_aCaP_sd"])
    }
    
    ### plotting
    p1 <- ggplot() +
        geom_point(data=pDF1, stat = "identity", 
                   aes(brk, value, col=CO2_trt),
                   position="stack", size=5) +
        geom_errorbar(data=pDF1, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos, col=CO2_trt), 
                      width=0.2, size=1) +
        ylab("eP RR") +
        scale_x_continuous(name = "Morphology",
                           breaks=c(1,2,3,4),
                           labels=c("Total root length", "SLA", 
                                    "LMA", "Leaf area"))+
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
              legend.position = c(0.1, 0.3))+
        geom_hline(yintercept=0, linetype=2)+
        ylim(-3,2)+
        scale_color_manual(name=expression(paste(CO[2], " treatment")),
                           breaks=c("aC", "eC"),
                           values=c("black","red"))
    
    p2 <- ggplot() +
        geom_point(data=pDF2, stat = "identity", 
                   aes(brk, value, col=P_trt),
                   position="stack", size=5) +
        geom_errorbar(data=pDF2, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos, col=P_trt), 
                      width=0.2, size=1) +
        ylab(expression(paste(eCO[2], " RR"))) +
        scale_x_continuous(name = "Morphology",
                           breaks=c(1,2,3,4),
                           labels=c("Total root length", "SLA", 
                                    "LMA", "Leaf area"))+
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
        ylim(-3,4)+
        scale_color_manual(name=paste("P treatment"),
                           breaks=c("aP", "eP"),
                           values=c("black","red"))
    
    p3 <- ggplot() +
        geom_point(data=pDF3, stat = "identity", 
                   aes(brk, value),
                   position="stack", size=5) +
        geom_errorbar(data=pDF3, 
                      mapping=aes(x=brk, ymin=neg, 
                                  ymax=pos), 
                      width=0.2, size=1) +
        ylab(expression(paste("Interaction RR"))) +
        scale_x_continuous(name = "Morphology",
                           breaks=c(1,2,3,4),
                           labels=c("Total root length", "SLA", 
                                    "LMA", "Leaf area"))+
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
        annotate("text", x=0.5, y=-2.7, label="n = ", size=5)+
        annotate("text", x=1, y=-2.7, label=n1, size=5)+
        annotate("text", x=2, y=-2.7, label=n2, size=5)+
        annotate("text", x=3, y=-2.7, label=n3, size=5)+
        annotate("text", x=4, y=-2.7, label=n4, size=5)
    
    ### summary histgram of treatments
    pdf("output/overall_morphology_100/summary_morphology_overall_plot.pdf", width=6, height=12)
    
    plot_grid(p1, p2, p3, 
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    

}