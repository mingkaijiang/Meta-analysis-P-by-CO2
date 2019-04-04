make_mean_effect_size_recalculation <- function(inDF) {
    ### Recalculates mean effect size and interaction effect size
    
    ### make numbers numeric
    inDF$Interaction_additive_aCaP <- as.numeric(as.character(inDF$Interaction_additive_aCaP))
    inDF$Interaction_multiplicative_aCaP <- as.numeric(as.character(inDF$Interaction_multiplicative_aCaP))
    
    inDF$Interaction_additive_aCeP <- as.numeric(as.character(inDF$Interaction_additive_aCeP))
    inDF$Interaction_multiplicative_aCeP <- as.numeric(as.character(inDF$Interaction_multiplicative_aCeP))
    
    inDF$Sample.Size <- as.numeric(as.character(inDF$Sample.Size))
    
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

    outDF <- inDF
    
    return(outDF)
    
}