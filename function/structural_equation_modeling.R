structural_equation_modeling_test <- function(reDF) {
    
    ### library
    require(lavaan)
    require(tidyr)
    
    ### clean the data
    sDF <- reDF[,c("Literature", "Category", "Variable", "Trt_aCO2", "Trt_eCO2",
                   "Trt_aP", "Trt_eP", "Sample.Size",
                   "Species", "Vegetation_type", "Soil_weight_in_pot",
                   "Pot_volume", "Experiment_duration", "Trt_eC_by_aC",
                   "Trt_eP_by_aP", "interaction", "log_interaction", "v_variance")]
    
    sDF$Variable <- gsub(" ", "_", sDF$Variable)
    
    wdDF <- spread(data = sDF, 
                 key = "Variable",
                 value = "log_interaction")
    
    
    ### structural equation
    HS.model <- 'Total_plant_biomass ~ Aboveground_biomass + Belowground_biomass
                 Aboveground_biomass ~ Leaf_biomass + Stem_biomass'
    
    fit <- cfa(HS.model, data = wdDF)
    
    
    ### plot correlation
    wilcox.test(wdDF$Aboveground_biomass, wdDF$Belowground_biomass, alternative = "two.sided")
    
    library("ggpubr")
    ggboxplot(sDF, x = "Category", y = "log_interaction", 
              color = "Category", 
              ylab = "log(r)", xlab = "Category")
    
    
    ### three-way anova
    test <- subset(sDF, Variable == "Leaf_biomass")
    testDF <- test[,c("Literature", "Vegetation_type", "Experiment_duration", "log_interaction")]
    testDF$Literature <- as.factor(testDF$Literature)
    testDF$Vegetation_type <- as.factor(testDF$Vegetation_type)
    testDF$Experiment_duration <- as.factor(testDF$Experiment_duration)
    
    plot.design(log_interaction ~ ., data = testDF)
    
    fm <- aov(log_interaction ~ Literature * Vegetation_type * Experiment_duration, data=testDF)
    summary(fm)
    
    ## obtain model without 3-way interaction
    fm <- update(fm, . ~ . -Literature:Vegetation_type:Experiment_duration)
    summary(fm)
    
    # remove 2-way anova
    fm1 <- update(fm, .~Literature+Vegetation_type+Experiment_duration)
    summary(fm1)
    
    # anova
    anova(fm, fm1)
    
    model.tables(fm1,type="effects")
    
    op <-  par(mfrow = c(2, 2))
    plot(fm1)
    par(op)
    
    
}