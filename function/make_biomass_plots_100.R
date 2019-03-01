make_biomass_plots_100 <- function(inDF) {
    
    if(!dir.exists("output/overall_biomass_100")) {
        dir.create("output/overall_biomass_100", showWarnings = FALSE)
    }
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
        xlab("Biomass") + 
        ylab("Phosphorus effect ratio") +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
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
        annotate("text", x=2, y=-2.20, label=paste0("n=", n5, ""), size=5)+
        annotate("text", x=1, y=-2.20, label=paste0("n=", n6, ""), size=5)+
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
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF1.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-2.50,5.00)+
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
        xlab("N content") + 
        ylab("Phosphorus effect ratio") +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-1.00,6.00)+
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
        annotate("text", x=5, y=-0.80, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=4, y=-0.80, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=3, y=-0.80, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-0.80, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=1, y=-0.80, label=paste0("n=", n5, ""), size=5)+
        annotate("rect", xmin = 1, xmax = 2, ymin = 4.00, ymax = 6.00,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=5.40, label=expression(aCO[2]), size=6)+
        annotate("text", x=1.3, y=5.40, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 4.10, yend = 4.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 4.10, yend = 4.50,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 4.10, yend = 4.10,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 4.50, yend = 4.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 4.10, yend = 4.10,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 4.50, yend = 4.50,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=4.30), color="black")+
        geom_point(aes(x=1.3, y=4.30), color="brown")
    
    
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
        ylab(expression(paste(CO[2], " effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-0.10,2.40)+
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
        annotate("rect", xmin = 1, xmax = 2, ymin = 1.70, ymax = 2.40,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=2.20, label=expression(aP), size=6)+
        annotate("text", x=1.3, y=2.20, label=expression(eP), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 175, yend = 2.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 1.75, yend = 2.00,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 1.75, yend = 1.75,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 2.00, yend = 2.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 1.75, yend = 1.75,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 2.00, yend = 2.00,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=1.875), color="black")+
        geom_point(aes(x=1.3, y=1.875), color="brown")
    
    
    p3 <- ggplot(bioDF2.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF2.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF2.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-0.50,1.00)+
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
        geom_point(data=bioDF2.sm, mapping=aes(x=Variable, y=Interaction_multiplicative_aCaP))+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-0.10,2.0)+
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
    pdf("output/overall_biomass_100/summary_N_content_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    ########## Make a further subset of the dataframe
    bioDF3 <- bioDF[bioDF$Variable %in% c("Total plant P content", "Leaf P content", "Stem P content",
                                          "Root P content", "Aboveground P content"),]
    
    bioDF3.sm <- compute_variable_mean_sd(bioDF3)
    
    ### Get sample size for each variable
    n1 <- length(bioDF3$Variable[bioDF3$Variable=="Total plant P content"])
    n2 <- length(bioDF3$Variable[bioDF3$Variable=="Stem P content"])
    n3 <- length(bioDF3$Variable[bioDF3$Variable=="Root P content"])
    n4 <- length(bioDF3$Variable[bioDF3$Variable=="Leaf P content"])
    n5 <- length(bioDF3$Variable[bioDF3$Variable=="Aboveground P content"])
    
    ### plot 
    p1 <- ggplot() +  
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                                  ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                                  ymax=eCeP_over_eCaP+eCeP_over_eCaP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF3.sm, stat = "identity", 
                   aes(Variable, aCeP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF3.sm, stat = "identity", 
                   aes(Variable, eCeP_over_eCaP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("P content") + 
        ylab("Phosphorus effect ratio") +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-6.50,15.00)+
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
        annotate("text", x=5, y=-5.80, label=paste0("n=", n1, ""), size=5)+
        annotate("text", x=4, y=-5.80, label=paste0("n=", n2, ""), size=5)+
        annotate("text", x=3, y=-5.80, label=paste0("n=", n3, ""), size=5)+
        annotate("text", x=2, y=-5.80, label=paste0("n=", n4, ""), size=5)+
        annotate("text", x=1, y=-5.80, label=paste0("n=", n5, ""), size=5)+
        annotate("rect", xmin = 1, xmax = 2, ymin = 8.00, ymax = 15.00,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=12.50, label=expression(aCO[2]), size=6)+
        annotate("text", x=1.3, y=12.50, label=expression(eCO[2]), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 8.50, yend = 10.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 8.50, yend = 10.50,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 8.50, yend = 8.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 10.50, yend = 10.50,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 8.50, yend = 8.50,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 10.50, yend = 10.50,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=9.50), color="black")+
        geom_point(aes(x=1.3, y=9.50), color="brown")
    
    p2 <- ggplot() +  
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=eCaP_over_aCaP-eCaP_over_aCaP_sd, 
                                                  ymax=eCaP_over_aCaP+eCaP_over_aCaP_sd), 
                      width=0.2, size=1, color="grey") + 
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCeP-eCeP_over_aCeP_sd, 
                                                  ymax=eCeP_over_aCeP+eCeP_over_aCeP_sd), 
                      width=0.2, size=1, color="red") + 
        geom_point(data=bioDF3.sm, stat = "identity", 
                   aes(Variable, eCaP_over_aCaP, fill=Variable),
                   position="stack") +
        geom_point(data=bioDF3.sm, stat = "identity", 
                   aes(Variable, eCeP_over_aCeP, fill=Variable),
                   position = "stack", col="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-0.10,2.40)+
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
        annotate("rect", xmin = 1, xmax = 2, ymin = 1.70, ymax = 2.40,
                 alpha = .2, color="black", fill="white")+
        annotate("text", x=1.7, y=2.20, label=expression(aP), size=6)+
        annotate("text", x=1.3, y=2.20, label=expression(eP), size=6)+
        annotate("segment", x = 1.7, xend = 1.7, y = 1.75, yend = 2.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.3, xend = 1.3, y = 1.75, yend = 2.00,
                 colour = "red", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 1.75, yend = 1.75,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.6, xend = 1.8, y = 2.00, yend = 2.00,
                 colour = "grey", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 1.75, yend = 1.75,
                 colour = "red", size=1)+
        annotate("segment", x = 1.2, xend = 1.4, y = 2.00, yend = 2.00,
                 colour = "red", size=1)+
        geom_point(aes(x=1.7, y=1.875), color="black")+
        geom_point(aes(x=1.3, y=1.875), color="brown")
    
    p3 <- ggplot(bioDF3.sm,
                 aes(Variable, Interaction_additive_aCaP)) +  
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=Interaction_additive_aCaP-Interaction_additive_aCaP_sd, 
                                                  ymax=Interaction_additive_aCaP+Interaction_additive_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF3.sm, mapping=aes(x=Variable, y=Interaction_additive_aCaP))+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-2.50,5.00)+
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
    
    p4 <- ggplot(bioDF3.sm,
                 aes(Variable, Interaction_multiplicative_aCaP)) +  
        geom_errorbar(data=bioDF3.sm, mapping=aes(x=Variable, ymin=Interaction_multiplicative_aCaP-Interaction_multiplicative_aCaP_sd, 
                                                  ymax=Interaction_multiplicative_aCaP+Interaction_multiplicative_aCaP_sd),
                      width=0.2, size=1, color="black") + 
        geom_point(data=bioDF3.sm, mapping=aes(x=Variable, y=Interaction_multiplicative_aCaP))+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))+
        theme_linedraw() +
        ylim(-0.10,2.00)+
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
    pdf("output/overall_biomass_100/summary_P_content_overall_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=4, align="h", axis = "l",
              rel_widths = c(1.2, 1.2,1,1))
    grid.text(grid.labs,x = c(0.065, 0.3, 0.57, 0.8),
              y = c(0.95, 0.95, 0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
}