make_concentration_plots_per_study <- function(inDF) {
    
    if(!dir.exists("output/individual_concentration")) {
        dir.create("output/individual_concentration", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Concentration")
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant N concentration", "Leaf N concentration", "Stem N concentration",
                                          "Root N concentration", "Aboveground N concentration"),]
    
    
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
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_concentration/Concentration_N_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    

    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf N concentration")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
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
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
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
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
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
    pdf("output/individual_concentration/summary_leaf_N_concentration_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    ########## Make a further subset of the dataframe
    bioDF2 <- bioDF[bioDF$Variable %in% c("Total plant P concentration", "Leaf P concentration", "Stem P concentration",
                                          "Root P concentration", "Aboveground P concentration"),]
    
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
        xlab("P concentration")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_concentration/Concentration_P_data_availability_plot.pdf")
    plot(p1)
    dev.off()
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF2, Variable=="Leaf P concentration")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
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
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
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
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
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
    pdf("output/individual_concentration/summary_leaf_P_concentration_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    
    ### Root 
    subDF1 <- subset(bioDF2, Variable=="Root P concentration")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
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
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
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
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
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
    pdf("output/individual_concentration/summary_root_P_concentration_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    

    ### Stem biomass 
    subDF1 <- subset(bioDF2, Variable=="Stem P concentration")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
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
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
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
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
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
    pdf("output/individual_concentration/summary_stem_P_concentration_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF2, Variable=="Total plant P concentration")
    
    ### plot individual study response ratio 
    p1 <- ggplot() +  
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, aCeP_over_aCaP, shape=Vegetation_type),
                   position="stack", size=3, color="black") +
        geom_point(data=subDF1, stat = "identity", 
                   aes(Literature, eCeP_over_eCaP, shape=Vegetation_type),
                   position = "stack", size=3, color="brown") +
        xlab("") + 
        ylab("Phosphorus effect (%)") +
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
        ylab(expression(paste(CO[2], " effect (%)"))) +
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
        ylab(expression(paste("Additive ", CO[2], " x P effect (%)"))) +
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
        ylab(expression(paste("Multiplicative ", CO[2], " x P effect (%)"))) +
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
    pdf("output/individual_concentration/summary_total_P_concentration_per_study_plot.pdf", width=16, height=5)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="auto", ncol=4, align="h", axis = "l", label_x=0.05, label_y=0.98,
              rel_widths = c(1.8,1,1,1.8))
    dev.off()
    
}