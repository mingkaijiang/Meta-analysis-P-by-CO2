compute_variable_mean_sd <- function(inDF) {
    DF.sm <- summaryBy(aCeP_over_aCaP+eCaP_over_aCaP+eCeP_over_aCaP+Interaction_additive_aCaP+Interaction_multiplicative_aCaP+
                           Interaction_additive_aCeP+Interaction_multiplicative_aCeP+Interaction_additive_aCaP_aCeP~Variable,
                       FUN=mean, data=inDF, keep.names=T, na.rm=T)
    DF.sm.sd <- summaryBy(aCeP_over_aCaP+eCaP_over_aCaP+eCeP_over_aCaP+Interaction_additive_aCaP+Interaction_multiplicative_aCaP+
                              Interaction_additive_aCeP+Interaction_multiplicative_aCeP+Interaction_additive_aCaP_aCeP~Variable,
                          FUN=sd, data=inDF, keep.names=T, na.rm=T)
    outDF <- merge(DF.sm, DF.sm.sd, by = "Variable", all.x=T, all.y=T)
    colnames(outDF) <- c("Variable","aCeP_over_aCaP","eCaP_over_aCaP","eCeP_over_aCaP",
                         "Interaction_additive_aCaP","Interaction_multiplicative_aCaP",
                         "Interaction_additive_aCeP","Interaction_multiplicative_aCeP",
                         "Interaction_additive_aCaP_aCeP",
                         "aCeP_over_aCaP_sd","eCaP_over_aCaP_sd","eCeP_over_aCaP_sd",
                         "Interaction_additive_aCaP_sd","Interaction_multiplicative_aCaP_sd",
                         "Interaction_additive_aCeP_sd","Interaction_multiplicative_aCeP_sd",
                         "Interaction_additive_aCaP_aCeP_sd")
    return(outDF)
}    

