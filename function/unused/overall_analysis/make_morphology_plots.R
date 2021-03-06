make_morphology_plots <- function(inDF) {
    
    if(!dir.exists("output/overall_morphology")) {
        dir.create("output/overall_morphology", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Morphology")
    
    ### change LAI to leaf area and combine it with Total leaf area
    bioDF[bioDF$Variable=="LAI","Variable"] <- "Leaf area"
    bioDF$Variable[bioDF$Variable=="Total leaf area"] <- "Leaf area"
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total root length", "SLA", 
                                          "Root to shoot ratio", "LMA", "Leaf area"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Total root length"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="SLA"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="Root to shoot ratio"])
    n4 <- length(bioDF1$Variable[bioDF1$Variable=="LMA"])
    n5 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf area"])
    
    ### plot 
    p1 <- ggplot() +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                                  ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                                  ymax=eCeP_over_eCaP+eCeP_over_eCaP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCeP_over_eCaP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        scale_x_discrete(name="Morphology", 
                         labels=c("Leaf Area", 
                                  "LMA", 
                                  "Root/shoot ratio",
                                  "SLA",
                                  "Root length"))+
        theme_linedraw() +
        ylim(-1.00,5.00)+
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
        annotate("text", x=5, y=-0.70, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=4, y=-0.70, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=3, y=-0.70, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-0.70, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=1, y=-0.70, label=paste0("n=", n5, ""), size=5)+
        annotate("rect", xmin = 2, xmax = 3, ymin = 2.00, ymax = 4.50,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=2.7, y=3.80, label=expression(aCO[2]), size=6)+
        annotate("text", x=2.3, y=3.80, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 2.7, xend = 2.7, y = 2.10, yend = 3.10,
                 colour = "grey", size=1)+
        annotate("segment", x = 2.3, xend = 2.3, y = 2.10, yend = 3.10,
                 colour = "red", size=1)+
        annotate("segment", x = 2.6, xend = 2.8, y = 2.10, yend = 2.10,
                 colour = "grey", size=1)+
        annotate("segment", x = 2.6, xend = 2.8, y = 3.10, yend = 3.10,
                 colour = "grey", size=1)+
        annotate("segment", x = 2.2, xend = 2.4, y = 2.10, yend = 2.10,
                 colour = "red", size=1)+
        annotate("segment", x = 2.2, xend = 2.4, y = 3.10, yend = 3.10,
                 colour = "red", size=1)+
        geom_point(aes(x=2.7, y=2.60), color="black")+
        geom_point(aes(x=2.3, y=2.60), color="brown")
    
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
        scale_x_discrete(name="Morphology", 
                         labels=c("Leaf Area", 
                                  "LMA", 
                                  "Root/shoot ratio",
                                  "SLA",
                                  "Root length"))+
        theme_linedraw() +
        ylim(-0.50,3.00)+
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
        annotate("rect", xmin = 1, xmax = 2, ymin = 1.80, ymax = 3.00,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=2.70, label=expression(aP), size=6)+
        annotate("text", x=1.3, y=2.70, label=expression(eP), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 1.90, yend = 2.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 1.90, yend = 2.50,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 1.90, yend = 1.90,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 2.50, yend = 2.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 1.90, yend = 1.90,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 2.50, yend = 2.50,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=2.20), color="black")+
        geom_point(aes(x=1.3, y=2.20), color="brown")
    

    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(name="Morphology", 
                         labels=c("Leaf Area", 
                                  "LMA", 
                                  "Root/shoot ratio",
                                  "SLA",
                                  "Root length"))+
        theme_linedraw() +
        ylim(-2.50,3.00)+
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
        geom_hline(yintercept=0, linetype=2)
    
    p4 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_multiplicative_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative_aCaP-Interaction_multiplicative_aCaP_sd, 
                                                  ymax=Interaction_multiplicative_aCaP+Interaction_multiplicative_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_multiplicative_aCaP))+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(name="Morphology", 
                         labels=c("Leaf Area", 
                                  "LMA", 
                                  "Root/shoot ratio",
                                  "SLA",
                                  "Root length"))+
        theme_linedraw() +
        ylim(-1,4.00)+
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
    pdf("output/overall_morphology/summary_morphology_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    

}