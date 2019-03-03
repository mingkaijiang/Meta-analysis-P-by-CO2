make_biomass_plots_100 <- function(inDF) {
    
    if(!dir.exists("output/overall_biomass_100")) {
        dir.create("output/overall_biomass_100", showWarnings = FALSE)
    }
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Biomass")
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant biomass", "Leaf biomass", "Stem biomass",
                                          "Root biomass", "Total plant N content", "Leaf N content",
                                          "Stem N content", "Root N content", "Total plant P content",
                                          "Leaf P content", "Stem P content", "Root P content"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    variable.list <- unique(bioDF1$Variable)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Total plant biomass"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="Stem biomass"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="Root biomass"])
    n4 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf biomass"])
    
    n5 <- length(bioDF1$Variable[bioDF1$Variable=="Total plant N content"])
    n6 <- length(bioDF1$Variable[bioDF1$Variable=="Stem N content"])
    n7 <- length(bioDF1$Variable[bioDF1$Variable=="Root N content"])
    n8 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf N content"])
    
    n9 <- length(bioDF1$Variable[bioDF1$Variable=="Total plant P content"])
    n10 <- length(bioDF1$Variable[bioDF1$Variable=="Stem P content"])
    n11 <- length(bioDF1$Variable[bioDF1$Variable=="Root P content"])
    n12 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf P content"])
    
    ### create a dataframe to contain long format data for each plot (3 dataframes)
    pDF1 <- data.frame(c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 
                         3.8, 4.2, 4.8, 5.2, 5.8, 6.2,
                         6.8, 7.2, 7.8, 8.2, 8.8, 9.2,
                         9.8, 10.2, 10.8, 11.2, 11.8, 12.2), NA, NA, NA, NA, NA)
    colnames(pDF1) <- c("brk", "variable", "CO2_trt", "value", "pos", "neg")
    pDF1$variable <- rep(c("Total plant biomass", "Stem biomass",
                           "Root biomass", "Leaf biomass",
                           "Total plant N content", "Stem N content",
                           "Root N content", "Leaf N content",
                           "Total plant P content", "Stem P content",
                           "Root P content", "Leaf P content"),2)
    pDF1$CO2_trt <- rep(c("aC", "eC"), each=12)
    pDF3 <- pDF2 <- pDF1
    
    ### assign values
    for (i in variable.list) {
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "value"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "aCeP_over_aCaP"]
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "pos"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "aCeP_over_aCaP"] + 
            bioDF1.sm[bioDF1.sm$Variable==i,
                      "aCeP_over_aCaP_sd"]
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="aC", "neg"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "aCeP_over_aCaP"] -
            bioDF1.sm[bioDF1.sm$Variable==i,
                      "aCeP_over_aCaP_sd"]
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "value"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                        "eCeP_over_eCaP"]
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "pos"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                      "eCeP_over_eCaP"] + 
            bioDF1.sm[bioDF1.sm$Variable==i,
                      "eCeP_over_eCaP_sd"]
        
        pDF1[pDF1$variable==i&pDF1$CO2_trt=="eC", "neg"] <- bioDF1.sm[bioDF1.sm$Variable==i,
                                                                      "eCeP_over_eCaP"] -
            bioDF1.sm[bioDF1.sm$Variable==i,
                      "eCeP_over_eCaP_sd"]
    }
    
    p1 <- ggplot() +
        
    

    ### plot 
    p1 <- ggplot() +  
        geom_errorbar(data=bioDF1.sm, 
                      mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                  ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF1.sm,
                      mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                  ymax=eCeP_over_eCaP+eCeP_over_eCaP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCeP_over_eCaP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("Biomass") + 
        ylab("Phosphorus effect ratio") +
        scale_x_discrete(labels=c("Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-5,15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=1, linetype=2)+
        annotate("text", x=6, y=-2.20, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=5, y=-2.20, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=4, y=-2.20, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=3, y=-2.20, label=paste0("n=", n4, ""), size=5)+
        #theme(legend.justification=c(1,0), legend.position=c(1,0))+
        annotate("rect", xmin = 5, xmax = 6, ymin = 8.00, ymax = 15.00,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=5.7, y=13.00, label=expression(aCO[2]), size=6)+
        annotate("text", x=5.3, y=13.00, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 5.7, xend = 5.7, y = 8.50, yend = 11.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.3, xend = 5.3, y = 8.50, yend = 11.00,
                 colour = "red", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 8.50, yend = 8.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 11.00, yend = 11.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 8.50, yend = 85.0,
                 colour = "red", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 11.00, yend = 11.00,
                 colour = "red", size=1)+
        geom_point(aes(x=5.7, y=9.75), color="black")+
        geom_point(aes(x=5.3, y=9.75), color="brown")
    
    plot(p1)
    
    p2 <- ggplot() +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCaP_over_aCaP-eCaP_over_aCaP_sd, 
                                                  ymax=eCaP_over_aCaP+eCaP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCeP-eCeP_over_aCeP_sd, 
                                                  ymax=eCeP_over_aCeP+eCeP_over_aCeP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCaP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCeP_over_aCeP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        scale_x_discrete(labels=c("Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-1,5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=1, linetype=2)+
        annotate("rect", xmin = 5, xmax = 6, ymin = 3.00, ymax = 5.00,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=5.7, y=4.40, label=expression(aP), size=6)+
        annotate("text", x=5.3, y=4.40, label=expression(eP), size=6)+
        annotate("segment", x = 5.7, xend = 5.7, y = 3.10, yend = 3.90,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.3, xend = 5.3, y = 3.10, yend = 3.90,
                 colour = "red", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 3.10, yend = 3.10,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 3.90, yend = 3.90,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 3.10, yend = 3.10,
                 colour = "red", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 3.90, yend = 3.90,
                 colour = "red", size=1)+
        geom_point(aes(x=5.7, y=3.50), color="black")+
        geom_point(aes(x=5.3, y=3.50), color="brown")
    
    

    
    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_multiplicative_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative_aCaP-Interaction_multiplicative_aCaP_sd, 
                                                  ymax=Interaction_multiplicative_aCaP+Interaction_multiplicative_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_multiplicative_aCaP))+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-1.00,2.50)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=1, linetype=2)
    
    
    
    ### summary histgram of treatments
    pdf("output/overall_biomass_100/summary_biomass_overall_plot.pdf", width=16, height=5)

    plot_grid(p1, p2, p3, 
              labels="AUTO", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    dev.off()
    
    

}