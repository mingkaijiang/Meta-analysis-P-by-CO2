make_nutrient_ratio_plots <- function(inDF) {
    
    if(!dir.exists("output/overall_nutrient_ratio")) {
        dir.create("output/overall_nutrient_ratio", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Nutrient Ratio")
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total NP ratio", "Leaf NP ratio", "Stem NP ratio",
                                          "Root NP ratio", "Aboveground NP ratio"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Total NP ratio"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="Stem NP ratio"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="Root NP ratio"])
    n4 <- length(bioDF1$Variable[bioDF1$Variable=="Leaf NP ratio"])
    n5 <- length(bioDF1$Variable[bioDF1$Variable=="Aboveground NP ratio"])
    
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
        scale_x_discrete(name="NP ratio", 
                         labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-50,200)+
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
        annotate("text", x=5, y=-20, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=4, y=-20, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=3, y=-20, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-20, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=1, y=-20, label=paste0("n=", n5, ""), size=5)+
        annotate("rect", xmin = 1, xmax = 2, ymin = 140, ymax = 200,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=180, label=expression(aCO[2]), size=6)+
        annotate("text", x=1.3, y=180, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 145, yend = 155,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 145, yend = 155,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 145, yend = 145,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 155, yend = 155,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 145, yend = 145,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 155, yend = 155,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=150), color="black")+
        geom_point(aes(x=1.3, y=150), color="brown")
    
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
        ylim(-10,160)+
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
        annotate("rect", xmin = 1, xmax = 2, ymin = 10, ymax = 60,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=50, label=expression(aP), size=6)+
        annotate("text", x=1.3, y=50, label=expression(eP), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 15, yend = 35,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 15, yend = 35,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 15, yend = 15,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 35, yend = 35,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 15, yend = 15,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 35, yend = 35,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=25), color="black")+
        geom_point(aes(x=1.3, y=25), color="brown")
    
    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
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
    
    p4 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_multiplicative_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative_aCaP-Interaction_multiplicative_aCaP_sd, 
                                                  ymax=Interaction_multiplicative_aCaP+Interaction_multiplicative_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_multiplicative_aCaP))+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
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
    pdf("output/overall_nutrient_ratio/summary_nutrient_ratio_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    

}