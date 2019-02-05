make_biomass_plots <- function(inDF) {
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Biomass")
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    
    ### Make a further subset of the dataframe
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
                                                  ymax=eCeP_over_eCaP+eCeP_over_eCaP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF1.sm, stat = "identity", 
                   aes(Variable, eCeP_over_eCaP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-500,1500)+
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
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=6, y=-220, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=5, y=-220, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=4, y=-220, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=3, y=-220, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=2, y=-220, label=paste0("n=", n5, ""), size=5)+
        annotate("text", x=1, y=-220, label=paste0("n=", n6, ""), size=5)+
        #theme(legend.justification=c(1,0), legend.position=c(1,0))+
        annotate("rect", xmin = 5, xmax = 6, ymin = 800, ymax = 1500,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=5.7, y=1300, label=expression(aCO[2]), size=6)+
        annotate("text", x=5.3, y=1300, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 5.7, xend = 5.7, y = 850, yend = 1100,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.3, xend = 5.3, y = 850, yend = 1100,
                 colour = "red", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 850, yend = 850,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 1100, yend = 1100,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 850, yend = 850,
                 colour = "red", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 1100, yend = 1100,
                 colour = "red", size=1)+
        geom_point(aes(x=5.7, y=975), color="black")+
        geom_point(aes(x=5.3, y=975), color="brown")
    
    
    
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-100,500)+
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
        geom_hline(yintercept=0, linetype=2)+
        annotate("rect", xmin = 5, xmax = 6, ymin = 300, ymax = 500,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=5.7, y=440, label=expression(aP), size=6)+
        annotate("text", x=5.3, y=440, label=expression(eP), size=6)+
        annotate("segment", x = 5.7, xend = 5.7, y = 310, yend = 390,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.3, xend = 5.3, y = 310, yend = 390,
                 colour = "red", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 310, yend = 310,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.6, xend = 5.8, y = 390, yend = 390,
                 colour = "grey", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 310, yend = 310,
                 colour = "red", size=1)+
        annotate("segment", x = 5.2, xend = 5.4, y = 390, yend = 390,
                 colour = "red", size=1)+
        geom_point(aes(x=5.7, y=350), color="black")+
        geom_point(aes(x=5.3, y=350), color="brown")
    
    
    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-250,500)+
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
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-100,150)+
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
    
    
    
    ### summary histgram of treatments
    pdf("output/summary_biomass_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    ########## Make a further subset of the dataframe
    bioDF2 <- bioDF[bioDF$Variable %in% c("Total plant N content", "Leaf N content", "Stem N content",
                                          "Root N content", "Aboveground N content"),]
    
    bioDF2.sm <- compute_variable_mean_sd(bioDF2)
    
    ### Get sample size for each variable
    n1 <- length(bioDF2$Variable[bioDF2$Variable=="Total plant N content"])
    n2 <- length(bioDF2$Variable[bioDF2$Variable=="Stem N content"])
    n3 <- length(bioDF2$Variable[bioDF2$Variable=="Root N content"])
    n4 <- length(bioDF2$Variable[bioDF2$Variable=="Leaf N content"])
    n5 <- length(bioDF2$Variable[bioDF2$Variable=="Aboveground N content"])
    
    ### plot 
    p1 <- ggplot() +  
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                                  ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                                  ymax=eCeP_over_eCaP+eCeP_over_eCaP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF2.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF2.sm, stat = "identity", 
                   aes(Variable, eCeP_over_eCaP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-100,600)+
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
        geom_hline(yintercept=0, linetype=2)+
        annotate("text", x=5, y=-80, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=4, y=-80, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=3, y=-80, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-80, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=1, y=-80, label=paste0("n=", n5, ""), size=5)+
        annotate("rect", xmin = 1, xmax = 2, ymin = 400, ymax = 600,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=540, label=expression(aCO[2]), size=6)+
        annotate("text", x=1.3, y=540, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 410, yend = 450,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 410, yend = 450,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 410, yend = 410,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 450, yend = 450,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 410, yend = 410,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 450, yend = 450,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=430), color="black")+
        geom_point(aes(x=1.3, y=430), color="brown")
    
    
    p2 <- ggplot() +  
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=eCaP_over_aCaP-eCaP_over_aCaP_sd, 
                                                  ymax=eCaP_over_aCaP+eCaP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCeP-eCeP_over_aCeP_sd, 
                                                  ymax=eCeP_over_aCeP+eCeP_over_aCeP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF2.sm, stat = "identity", 
                   aes(Variable, eCaP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF2.sm, stat = "identity", 
                   aes(Variable, eCeP_over_aCeP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-10,240)+
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
        geom_hline(yintercept=0, linetype=2)+
        annotate("rect", xmin = 1, xmax = 2, ymin = 170, ymax = 240,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=220, label=expression(aP), size=6)+
        annotate("text", x=1.3, y=220, label=expression(eP), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 175, yend = 200,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 175, yend = 200,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 175, yend = 175,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 200, yend = 200,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 175, yend = 175,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 200, yend = 200,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=187.5), color="black")+
        geom_point(aes(x=1.3, y=187.5), color="brown")
    
    
    p3 <- ggplot(bioDF2.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-50,100)+
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
    
    p4 <- ggplot(bioDF2.sm,
                 aes(Variable, Interaction_multiplicative_aCaP)) +  
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative_aCaP-Interaction_multiplicative_aCaP_sd, 
                                                  ymax=Interaction_multiplicative_aCaP+Interaction_multiplicative_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-50,50)+
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
    
    
    
    ### summary histgram of treatments
    pdf("output/summary_N_content_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
}