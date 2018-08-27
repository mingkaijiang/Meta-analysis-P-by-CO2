##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

##### Master script
### source
source("prepare.R")

#### check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V1.csv",strip.white=T)

myDF <- as.data.frame(myDF)

myDF$Interaction_additive <- as.numeric(as.character(myDF$Interaction_additive))
myDF$Interaction_multiplicative <- as.numeric(as.character(myDF$Interaction_multiplicative))

### Calculate how much CO2 rise there is
myDF$Trt_eC_by_aC <- myDF$Trt_eCO2/myDF$Trt_aCO2

### Calculate how much P reduction there is
myDF$Trt_P_reduction <- (myDF$Trt_eP - myDF$Trt_aP) / myDF$Trt_eP

### Plotting

### summary histgram of treatments
hist(myDF$Trt_aCO2, xlab = "aCO2 (ppm)", main=NA)
hist(myDF$Trt_eCO2, xlab = "eCO2 (ppm)", main=NA)
hist(myDF$Trt_eC_by_aC, xlab = "eCO2/aCO2", main=NA)
hist(myDF$Trt_P_reduction, xlab = "eP/aP", main=NA)

### Subset biomass category
bioDF <- subset(myDF, Category == "Biomass")
bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant biomass", "Leaf biomass", "Stem biomass",
                                      "Root biomass", "Aboveground biomass", "Belowground biomass"),]

bioDF1.sm <- compute_variable_mean_sd(bioDF1)


### plot 
p1 <- ggplot(bioDF1.sm,
             aes(Variable, aCeP_over_aCaP)) +  
    geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=aCeP_over_aCaP-aCeP_over_aCaP_sd, 
                                              ymax=aCeP_over_aCaP+aCeP_over_aCaP_sd), 
                  width=0.2, size=1, color="grey") + 
    geom_point(stat = "identity", aes(fill=Variable),
               position="stack") +
    xlab("") + ylab("P effect ratio") +
    scale_x_discrete(labels=c("AG", 
                              "BG",
                              "Leaf", 
                              "Root",
                              "Stem",
                              "Total"))+
    theme_linedraw() +
    #ylim(-50,150)+
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
    geom_hline(yintercept=1, linetype=2)

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
    #ylim(-50,150)+
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
    geom_hline(yintercept=1, linetype=2)


p3 <- ggplot(bioDF1.sm,
             aes(Variable, eCeP_over_aCaP)) +  
    geom_errorbar(data=bioDF1.sm, mapping=aes(x=Variable, ymin=eCeP_over_aCaP-eCeP_over_aCaP_sd, 
                                              ymax=eCeP_over_aCaP+eCeP_over_aCaP_sd), 
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
    #ylim(-50,150)+
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
    geom_hline(yintercept=1, linetype=2)

plot(p3)
