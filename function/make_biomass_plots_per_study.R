make_biomass_plots_per_study <- function(inDF) {
    
    if(!dir.exists("output/individual_biomass")) {
        dir.create("output/individual_biomass", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Biomass")
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant biomass", "Leaf biomass", "Stem biomass",
                                          "Root biomass", "Aboveground biomass", "Belowground biomass"),]
    
    
    ### check how many studies are available for each data variable
    varDF <- unique(bioDF1[c("Literature", "Variable", "Vegetation_type")])
    
    p1 <- ggplot()+
        geom_bar(data=varDF, aes(Variable, fill=Vegetation_type),stat="count")+
        theme_linedraw() +
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
        ylab("Study count")+
        xlab("Biomass")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(labels=c("AG", 
                                  "BG",
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_biomass/Biomass_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground biomass")

    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_aboveground_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    ### Belowground biomass 
    subDF1 <- subset(bioDF1, Variable=="Belowground biomass")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_belowground_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf biomass")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_leaf_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem biomass")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_stem_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root biomass")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_root_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant biomass")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_total_biomass_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    
    ########## Make a further subset of the dataframe
    bioDF2 <- bioDF[bioDF$Variable %in% c("Total plant N content", "Leaf N content", "Stem N content",
                                          "Root N content", "Aboveground N content"),]
    
    ### check how many studies are available for each data variable
    varDF <- unique(bioDF2[c("Literature", "Variable", "Vegetation_type")])
    
    p1 <- ggplot()+
        geom_bar(data=varDF, aes(Variable, fill=Vegetation_type),stat="count")+
        theme_linedraw() +
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
        ylab("Study count")+
        xlab("N content")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_biomass/Biomass_N_content_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF2, Variable=="Leaf N content")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_leaf_N_content_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    

    ########## Make a further subset of the dataframe
    bioDF3 <- bioDF[bioDF$Variable %in% c("Total plant P content", "Leaf P content", "Stem P content",
                                          "Root P content", "Aboveground P content"),]
    
    ### check how many studies are available for each data variable
    varDF <- unique(bioDF3[c("Literature", "Variable", "Vegetation_type")])
    
    p1 <- ggplot()+
        geom_bar(data=varDF, aes(Variable, fill=Vegetation_type),stat="count")+
        theme_linedraw() +
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
        ylab("Study count")+
        xlab("P content")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_biomass/Biomass_P_content_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF3, Variable=="Aboveground P content")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_aboveground_P_content_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF3, Variable=="Leaf P content")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_leaf_P_content_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Stem biomass 
    subDF1 <- subset(bioDF3, Variable=="Stem P content")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_stem_P_content_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF3, Variable=="Root P content")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect ratio") +
        theme_linedraw() +
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
        geom_hline(yintercept=0, linetype=2) 
    
    p2 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCaP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_aCeP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab(expression(paste(CO[2], " effect ratio"))) +
        theme_linedraw() +
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
    
    
    p3 <- ggplot(subDF1,
                 aes(Literature, Interaction_additive_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_additive_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Additive ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
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
    
    p4 <- ggplot(subDF1,
                 aes(Literature, Interaction_multiplicative_aCaP)) +  
        geom_point(data=subDF1, mapping=aes(x=Literature, y=Interaction_multiplicative_aCaP, shape=Vegetation_type), size=3)+
        xlab("") + 
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect ratio"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        coord_flip()+
        geom_hline(yintercept=0, linetype=2)+
        scale_shape(name="Vegetation type") 
    
    
    
    ### summary histgram of treatments
    pdf("output/individual_biomass/summary_root_P_content_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
}