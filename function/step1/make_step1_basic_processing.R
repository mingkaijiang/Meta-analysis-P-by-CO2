make_step1_basic_processing <- function(inDF) {
    ### Recalculates mean effect size and interaction effect size
    
    inDF <- as.data.frame(inDF)
    
    ### make numbers numeric
    inDF$Sample.Size <- as.numeric(as.character(inDF$Sample.Size))
    
    ### check CO2 treatment ratio
    inDF$Trt_eC_by_aC <- inDF$Trt_eCO2/inDF$Trt_aCO2
    
    ### check P treatment ratio
    inDF$Trt_eP_by_aP <- inDF$Trt_eP / inDF$Trt_aP
    
    
    ### Exclude some aCO2 treatment beyond current CO2 concentrations
    inDF <- subset(inDF, Trt_aCO2 < 410)
    
    
    ### Calculates a range of mean effect size, including those not included in the original dataset
    ### unit: 100%
    
    ## over aCaP
    inDF$aCaP_over_aCaP <- (inDF$aCaP_mean / inDF$aCaP_mean) 
    inDF$aCeP_over_aCaP <- (inDF$aCeP_mean / inDF$aCaP_mean) 
    inDF$eCaP_over_aCaP <- (inDF$eCaP_mean / inDF$aCaP_mean) 
    inDF$eCeP_over_aCaP <- (inDF$eCeP_mean / inDF$aCaP_mean) 
    
    ## over aCeP
    inDF$aCaP_over_aCeP <- (inDF$aCaP_mean / inDF$aCeP_mean) 
    inDF$aCeP_over_aCeP <- (inDF$aCeP_mean / inDF$aCeP_mean) 
    inDF$eCaP_over_aCeP <- (inDF$eCaP_mean / inDF$aCeP_mean) 
    inDF$eCeP_over_aCeP <- (inDF$eCeP_mean / inDF$aCeP_mean) 
    
    ## over eCaP
    inDF$aCaP_over_eCaP <- (inDF$aCaP_mean / inDF$eCaP_mean) 
    inDF$aCeP_over_eCaP <- (inDF$aCeP_mean / inDF$eCaP_mean) 
    inDF$eCaP_over_eCaP <- (inDF$eCaP_mean / inDF$eCaP_mean) 
    inDF$eCeP_over_eCaP <- (inDF$eCeP_mean / inDF$eCaP_mean) 
    
    ## over eCeP
    inDF$aCaP_over_eCeP <- (inDF$aCaP_mean / inDF$eCeP_mean) 
    inDF$aCeP_over_eCeP <- (inDF$aCeP_mean / inDF$eCeP_mean) 
    inDF$eCaP_over_eCeP <- (inDF$eCaP_mean / inDF$eCeP_mean) 
    inDF$eCeP_over_eCeP <- (inDF$eCeP_mean / inDF$eCeP_mean) 
    
    ### Compute interaction terms, additive
    inDF$Interaction_additive_aCaP <- (inDF$eCeP_over_aCaP - 1) - (inDF$eCaP_over_aCaP - 1) - (inDF$aCeP_over_aCaP - 1)
    inDF$Interaction_additive_aCeP <- (inDF$eCaP_over_aCeP - 1) - (inDF$eCeP_over_aCeP - 1) - (inDF$aCaP_over_aCeP - 1)
    
    ### Compute interaction terms, multiplicative
    inDF$Interaction_multiplicative_aCaP <- ((inDF$eCeP_mean / inDF$eCaP_mean) / (inDF$aCeP_mean / inDF$aCaP_mean)) 
    inDF$Interaction_multiplicative_aCeP <- ((inDF$eCaP_mean / inDF$aCaP_mean) / (inDF$eCeP_mean / inDF$aCeP_mean)) 
    
    
    ### check number of rows
    nrow <- nrow(inDF)
    
    ### split the data frame
    myDF1 <- subset(inDF, Confidence.interval.type=="SE") 
    #myDF2 <- subset(inDF, Confidence.interval.type=="LSD") 
    myDF3 <- subset(inDF, Confidence.interval.type=="SD") 
    #myDF4 <- subset(inDF, Confidence.interval.type=="2SE") 
    myDF5 <- subset(inDF, !Confidence.interval.type%in%c("SE", "LSD", "SD", "2SE")) 
    
    ### Convert SD to SE
    myDF3$aCaP_pos <- myDF3$aCaP_mean + (myDF3$aCaP_pos - myDF3$aCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCaP_neg <- myDF3$aCaP_mean - (myDF3$aCaP_mean - myDF3$aCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCeP_pos <- myDF3$aCeP_mean + (myDF3$aCeP_pos - myDF3$aCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCeP_neg <- myDF3$aCeP_mean - (myDF3$aCeP_mean - myDF3$aCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCaP_pos <- myDF3$eCaP_mean + (myDF3$eCaP_pos - myDF3$eCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCaP_neg <- myDF3$eCeP_mean - (myDF3$eCaP_mean - myDF3$eCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    
    myDF3$eCeP_pos <- myDF3$eCeP_mean + (myDF3$eCeP_pos - myDF3$eCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCeP_neg <- myDF3$eCeP_mean - (myDF3$eCeP_mean - myDF3$eCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$Confidence.interval.type <- "SE"
    
    
    ### No CI information available, consider it as SE
    myDF5$Confidence.interval.type <- "SE"
    
    ### return
    outDF1 <- rbind(myDF1, myDF3, myDF5)
    
    ### add sd information 
    outDF1$aCaP_sd <- (outDF1$aCaP_pos - outDF1$aCaP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$eCaP_sd <- (outDF1$eCaP_pos - outDF1$eCaP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$aCeP_sd <- (outDF1$aCeP_pos - outDF1$aCeP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$eCeP_sd <- (outDF1$eCeP_pos - outDF1$eCeP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    
    
    ### return
    return(outDF1)
    
}