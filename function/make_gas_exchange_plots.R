make_gas_exchange_plots <- function(inDF) {
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Gas Exchange")
    
    bioDF.sm <- compute_variable_mean_sd(bioDF)
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Asat", "CO2 assimilation rate", 
                                          "Stomatal conductance"),]
    
    bioDF1.sm <- compute_variable_mean_sd(bioDF1)
    
    ### Get sample size for each variable
    n1 <- length(bioDF1$Variable[bioDF1$Variable=="Asat"])
    n2 <- length(bioDF1$Variable[bioDF1$Variable=="CO2 assimilation rate"])
    n3 <- length(bioDF1$Variable[bioDF1$Variable=="Stomatal conductance"])
    
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
        scale_x_discrete(labels=c(expression(A[sat]), 
                                  expression(A[inst]), 
                                  expression(g[s])))+
        theme_linedraw() +
        ylim(-100,500)+
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
        annotate("text", x=3, y=-70, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-70, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=1, y=-70, label=paste0("n=", n1, ""), size=5)+
        annotate("rect", xmin = 0.8, xmax = 1.5, ymin = 250, ymax = 480,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.3, y=420, label=expression(aCO[2]), size=6)+
        annotate("text", x=1.0, y=420, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 1.3, xend = 1.3, y = 270, yend = 370,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.0, xend = 1.0, y = 270, yend = 370,
                 colour = "red", size=1)+
        annotate("segment", x = 1.25, xend = 1.35, y = 270, yend = 270,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.25, xend = 1.35, y = 370, yend = 370,
                 colour = "grey", size=1)+
        annotate("segment", x = 0.95, xend = 1.05, y = 270, yend = 270,
                 colour = "red", size=1)+
        annotate("segment", x = 0.95, xend = 1.05, y = 370, yend = 370,
                 colour = "red", size=1)+
        geom_point(aes(x=1.3, y=320), color="black")+
        geom_point(aes(x=1.0, y=320), color="brown")
    
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
        scale_x_discrete(labels=c(expression(A[sat]), 
                                  expression(A[inst]), 
                                  expression(g[s])))+
        theme_linedraw() +
        ylim(-10,200)+
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
        annotate("rect", xmin = 0.8, xmax = 1.5, ymin = 120, ymax = 200,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.3, y=180, label=expression(aP), size=6)+
        annotate("text", x=1, y=180, label=expression(eP), size=6)+
        annotate("segment", x = 1.3, xend = 1.3, y = 130, yend = 160,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.0, xend = 1.0, y = 130, yend = 160,
                 colour = "red", size=1)+
        annotate("segment", x = 1.25, xend = 1.35, y = 130, yend = 130,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.25, xend = 1.35, y = 160, yend = 160,
                 colour = "grey", size=1)+
        annotate("segment", x = 0.95, xend = 1.05, y = 130, yend = 130,
                 colour = "red", size=1)+
        annotate("segment", x = 0.95, xend = 1.05, y = 160, yend = 160,
                 colour = "red", size=1)+
        geom_point(aes(x=1.3, y=145), color="black")+
        geom_point(aes(x=1.0, y=145), color="brown")
    
    p3 <- ggplot(bioDF1.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
        scale_x_discrete(labels=c(expression(A[sat]), 
                                  expression(A[inst]), 
                                  expression(g[s])))+
        theme_linedraw() +
        ylim(-100,100)+
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
        scale_x_discrete(labels=c(expression(A[sat]), 
                                  expression(A[inst]), 
                                  expression(g[s])))+
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
    pdf("output/summary_gas_exchange_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    

}