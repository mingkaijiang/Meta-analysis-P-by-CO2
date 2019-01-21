make_basic_stats_plots <- function(myDF) {
    ### This function processes the dataframe with some basic summaries
    
    ### check ratio of CO2 treatment
    myDF$Trt_eC_by_aC <- myDF$Trt_eCO2/myDF$Trt_aCO2
    
    ### check P treatment
    myDF$Trt_eP_by_aP <- myDF$Trt_eP / myDF$Trt_aP
    
    ### check P reduction ratio
    myDF$Trt_P_reduction <- (myDF$Trt_eP - myDF$Trt_aP) / myDF$Trt_eP
    
    ### Exclude some extremely high P addition experiment
    #test <- subset(myDF, Trt_P_reduction <= 0.9)
    test2 <- subset(myDF, Vegetation_type == "Grass Forb Legume")
    
    myDF2 <- myDF
    
    ### Plotting
    
    ### summary histgram of treatments
    hist(myDF2$Trt_aCO2, xlab = "aCO2 (ppm)", main=NA)
    hist(myDF2$Trt_eCO2, xlab = "eCO2 (ppm)", main=NA)
    hist(myDF2$Trt_eC_by_aC, xlab = "eCO2/aCO2", main=NA)
    hist(myDF2$Trt_P_reduction, xlab = "eP/aP", main=NA)
    
    ### Subset biomass category
    bioDF <- subset(myDF2, Category == "Biomass")
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant biomass", "Leaf biomass", "Stem biomass",
                                          "Root biomass", "Aboveground biomass", "Belowground biomass"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Total plant biomass"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="Stem biomass"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="Root biomass"])
    n4 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf biomass"])
    n5 <- length(bioDF1$Variable[bioDF1$Variable=="Belowground biomass"])
    n6 <- length(bioDF1$Variable[bioDF1$Variable=="Aboveground biomass"])
    
    
    ### plot 
    p1 <- ggplot() +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                                  ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                                  ymax=eCeP_over_aCaP+eCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="cyan") + 
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCeP_over_aCaP, fill=Variable),
                   position = "stack", col="blue") +
        xlab("") + ylab("P effect ratio") +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-5,25)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=6, label=paste0("(n=", n1, ")"), size=5)+
        annotate("text", x=5, y=8, label=paste0("(n=", n2, ")"), size=5)+
        annotate("text", x=4, y=6, label=paste0("(n=", n3, ")"), size=5)+
        annotate("text", x=3, y=17, label=paste0("(n=", n4, ")"), size=5)+
        annotate("text", x=2, y=6, label=paste0("(n=", n5, ")"), size=5)+
        annotate("text", x=1, y=6, label=paste0("(n=", n6, ")"), size=5)
    
    plot(p1)
    
    p2 <- ggplot(bioDF1.sm,
                 aes(Variable, eCaP_over_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCaP_over_aCaP-eCaP_over_aCaP_sd, 
                                                  ymax=eCaP_over_aCaP+eCaP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_point(stat = "identity", aes(fill=Variable),
                   position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " effect ratio at aP"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-1,5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=3, label=paste0("(n=", n1, ")"), size=5)+
        annotate("text", x=5, y=3, label=paste0("(n=", n2, ")"), size=5)+
        annotate("text", x=4, y=3, label=paste0("(n=", n3, ")"), size=5)+
        annotate("text", x=3, y=4, label=paste0("(n=", n4, ")"), size=5)+
        annotate("text", x=2, y=3, label=paste0("(n=", n5, ")"), size=5)+
        annotate("text", x=1, y=3, label=paste0("(n=", n6, ")"), size=5)
    
    plot(p2)
    
    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, eCeP_over_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                                  ymax=eCeP_over_aCaP+eCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_point(stat = "identity", aes(fill=Variable),
                   position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " effect ratio at eP"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-5,26)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=10, label=paste0("(n=", n1, ")"), size=5)+
        annotate("text", x=5, y=10, label=paste0("(n=", n2, ")"), size=5)+
        annotate("text", x=4, y=10, label=paste0("(n=", n3, ")"), size=5)+
        annotate("text", x=3, y=25.5, label=paste0("(n=", n4, ")"), size=5)+
        annotate("text", x=2, y=10, label=paste0("(n=", n5, ")"), size=5)+
        annotate("text", x=1, y=12, label=paste0("(n=", n6, ")"), size=5)
    
    plot(p3)
    
    p4 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_additive)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive-Interaction_additive_sd, 
                                                  ymax=Interaction_additive+Interaction_additive_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_point(stat = "identity", aes(fill=Variable),
                   position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " x P interaction (additive)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-4,10)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=4, label=paste0("(n=", n1, ")"), size=5)+
        annotate("text", x=5, y=4, label=paste0("(n=", n2, ")"), size=5)+
        annotate("text", x=4, y=4, label=paste0("(n=", n3, ")"), size=5)+
        annotate("text", x=3, y=9, label=paste0("(n=", n4, ")"), size=5)+
        annotate("text", x=2, y=4, label=paste0("(n=", n5, ")"), size=5)+
        annotate("text", x=1, y=6, label=paste0("(n=", n6, ")"), size=5)
    
    plot(p4)
    
    p5 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_multiplicative)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative-Interaction_multiplicative_sd, 
                                                  ymax=Interaction_multiplicative+Interaction_multiplicative_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_point(stat = "identity", aes(fill=Variable),
                   position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " x P interaction (multiplicative)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-1,3)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=1.5, label=paste0("(n=", n1, ")"), size=5)+
        annotate("text", x=5, y=1.5, label=paste0("(n=", n2, ")"), size=5)+
        annotate("text", x=4, y=1.5, label=paste0("(n=", n3, ")"), size=5)+
        annotate("text", x=3, y=2.5, label=paste0("(n=", n4, ")"), size=5)+
        annotate("text", x=2, y=1.5, label=paste0("(n=", n5, ")"), size=5)+
        annotate("text", x=1, y=1.5, label=paste0("(n=", n6, ")"), size=5)
    
    plot(p5)
    
    
}