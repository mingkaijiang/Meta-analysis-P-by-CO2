metafor_statistics_nutrient_uptake_investigation <- function() {
    
    ### create directory
    if(!dir.exists("output/statistics_nutrient_uptake_100")) {
        dir.create("output/statistics_nutrient_uptake_100", showWarnings = FALSE)
    }
    
    reDF <- reDF100
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant N uptake")
    
    tDF1 <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    tDF2 <- subset(tDF, Unit == "mg plant-1")
    
    ### per root
    res1 <- rma(log_interaction, v_variance, data = tDF1)
    res1$b
    res1$se
    res1$pval
    
    ### total plant
    res2 <- rma(log_interaction, v_variance, data = tDF2)
    res2$b
    res2$se
    res2$pval
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant P uptake")
    
    tDF1 <- subset(tDF, Unit %in%c("ug m-1 root", "mg g-1 root", "mg P g-1 root"))
    tDF2 <- subset(tDF, Unit == "mg plant-1")
    
    ### per root
    res1 <- rma(log_interaction, v_variance, data = tDF1)
    res1$b
    res1$se
    res1$pval
    dim(tDF1)
    
    ### total plant
    res2 <- rma(log_interaction, v_variance, data = tDF2)
    res2$b
    res2$se
    res2$pval
    dim(tDF2)
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    confint(res)
    res$b
    res$se
    res$pval
    
    

}