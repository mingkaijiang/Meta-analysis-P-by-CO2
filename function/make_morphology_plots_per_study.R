make_morphology_plots_per_study <- function(inDF) {
    
    if(!dir.exists("output/individual_morphology")) {
        dir.create("output/individual_morphology", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Morphology")
    
    ### change LAI to leaf area and combine it with Total leaf area
    bioDF[bioDF$Variable=="LAI","Variable"] <- "Leaf area"
    bioDF$Variable[bioDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total root length", "SLA", 
                                          "Root to shoot ratio", "LMA", "Leaf area"),]
    
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
        xlab("N concentration")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(name="Morphology", 
                         labels=c("Leaf Area", 
                                  "LMA", 
                                  "Root/shoot ratio",
                                  "SLA",
                                  "Root length"))
    
    pdf("output/individual_morphology/Morphology_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    

    ### Leaf area
    subDF1 <- subset(bioDF1, Variable=="Leaf area")
    
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
    pdf("output/individual_morphology/summary_leaf_area_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    
    ### LMA
    subDF1 <- subset(bioDF1, Variable=="LMA")
    
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
    pdf("output/individual_morphology/summary_LMA_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    ### SLA
    subDF1 <- subset(bioDF1, Variable=="SLA")
    
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
    pdf("output/individual_morphology/summary_SLA_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    

    ### Root length
    subDF1 <- subset(bioDF1, Variable=="Total root length")
    
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
    pdf("output/individual_morphology/summary_root_length_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    
}