metafor_statistics_nonwoody_plants_100 <- function(reDF, intDF) {
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_biomass"] <- res$b
    intDF$se[intDF$variable=="leaf_biomass"] <- res$se
    intDF$p_value[intDF$variable=="leaf_biomass"] <- res$pval
    intDF$ns[intDF$variable=="leaf_biomass"] <- ns
    intDF$ne[intDF$variable=="leaf_biomass"] <- l
    intDF$ci_lb[intDF$variable=="leaf_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_biomass"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="stem_biomass"] <- res$b
    intDF$se[intDF$variable=="stem_biomass"] <- res$se
    intDF$p_value[intDF$variable=="stem_biomass"] <- res$pval
    intDF$ns[intDF$variable=="stem_biomass"] <- ns
    intDF$ne[intDF$variable=="stem_biomass"] <- l
    intDF$ci_lb[intDF$variable=="stem_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stem_biomass"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root biomass")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="root_biomass"] <- res$b
    intDF$se[intDF$variable=="root_biomass"] <- res$se
    intDF$p_value[intDF$variable=="root_biomass"] <- res$pval
    intDF$ns[intDF$variable=="root_biomass"] <- ns
    intDF$ne[intDF$variable=="root_biomass"] <- l
    intDF$ci_lb[intDF$variable=="root_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_biomass"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, v_variance >= 0.01)
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="total_biomass"] <- res$b
    intDF$se[intDF$variable=="total_biomass"] <- res$se
    intDF$p_value[intDF$variable=="total_biomass"] <- res$pval
    intDF$ns[intDF$variable=="total_biomass"] <- ns
    intDF$ne[intDF$variable=="total_biomass"] <- l
    intDF$ci_lb[intDF$variable=="total_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="total_biomass"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Aboveground biomass")
    tDF <- subset(tDF, v_variance >= 0.01)
    
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="aboveground_biomass"] <- res$b
    intDF$se[intDF$variable=="aboveground_biomass"] <- res$se
    intDF$p_value[intDF$variable=="aboveground_biomass"] <- res$pval
    intDF$ns[intDF$variable=="aboveground_biomass"] <- ns
    intDF$ne[intDF$variable=="aboveground_biomass"] <- l
    intDF$ci_lb[intDF$variable=="aboveground_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="aboveground_biomass"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Root biomass", "Belowground biomass"))
    
    ### random-effect model
    tDF <- subset(tDF, v_variance >= 0.001)
    tDF <- subset(tDF, v_variance <= 2)
    
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="belowground_biomass"] <- res$b
    intDF$se[intDF$variable=="belowground_biomass"] <- res$se
    intDF$p_value[intDF$variable=="belowground_biomass"] <- res$pval
    intDF$ns[intDF$variable=="belowground_biomass"] <- ns
    intDF$ne[intDF$variable=="belowground_biomass"] <- l
    intDF$ci_lb[intDF$variable=="belowground_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="belowground_biomass"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N content")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF, digits=5, 
               control=list(stepadj=0.05))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_N_content"] <- res$b
    intDF$se[intDF$variable=="leaf_N_content"] <- res$se
    intDF$p_value[intDF$variable=="leaf_N_content"] <- res$pval
    intDF$ns[intDF$variable=="leaf_N_content"] <- ns
    intDF$ne[intDF$variable=="leaf_N_content"] <- l
    intDF$ci_lb[intDF$variable=="leaf_N_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_N_content"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P content")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_P_content"] <- res$b
    intDF$se[intDF$variable=="leaf_P_content"] <- res$se
    intDF$p_value[intDF$variable=="leaf_P_content"] <- res$pval
    intDF$ns[intDF$variable=="leaf_P_content"] <- ns
    intDF$ne[intDF$variable=="leaf_P_content"] <- l
    intDF$ci_lb[intDF$variable=="leaf_P_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_P_content"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_N_concentration"] <- res$b
    intDF$se[intDF$variable=="leaf_N_concentration"] <- res$se
    intDF$p_value[intDF$variable=="leaf_N_concentration"] <- res$pval
    intDF$ns[intDF$variable=="leaf_N_concentration"] <- ns
    intDF$ne[intDF$variable=="leaf_N_concentration"] <- l
    intDF$ci_lb[intDF$variable=="leaf_N_concentration"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_N_concentration"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$v_variance > 0, ]
    
    res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_P_concentration"] <- res$b
    intDF$se[intDF$variable=="leaf_P_concentration"] <- res$se
    intDF$p_value[intDF$variable=="leaf_P_concentration"] <- res$pval
    intDF$ns[intDF$variable=="leaf_P_concentration"] <- ns
    intDF$ne[intDF$variable=="leaf_P_concentration"] <- l
    intDF$ci_lb[intDF$variable=="leaf_P_concentration"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_P_concentration"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$v_variance > 0, ]
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="root_P_concentration"] <- res$b
    intDF$se[intDF$variable=="root_P_concentration"] <- res$se
    intDF$p_value[intDF$variable=="root_P_concentration"] <- res$pval
    intDF$ns[intDF$variable=="root_P_concentration"] <- ns
    intDF$ne[intDF$variable=="root_P_concentration"] <- l
    intDF$ci_lb[intDF$variable=="root_P_concentration"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_P_concentration"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N concentration")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    intDF$interaction[intDF$variable=="root_N_concentration"] <- res$b
    intDF$se[intDF$variable=="root_N_concentration"] <- res$se
    intDF$p_value[intDF$variable=="root_N_concentration"] <- res$pval
    intDF$ns[intDF$variable=="root_N_concentration"] <- ns
    intDF$ne[intDF$variable=="root_N_concentration"] <- l
    intDF$ci_lb[intDF$variable=="root_N_concentration"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_N_concentration"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="CO2_assimilation_rate"] <- res$b
    intDF$se[intDF$variable=="CO2_assimilation_rate"] <- res$se
    intDF$p_value[intDF$variable=="CO2_assimilation_rate"] <- res$pval
    intDF$ns[intDF$variable=="CO2_assimilation_rate"] <- ns
    intDF$ne[intDF$variable=="CO2_assimilation_rate"] <- l
    intDF$ci_lb[intDF$variable=="CO2_assimilation_rate"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="CO2_assimilation_rate"] <- res$ci.ub
    
    ### change LAI to leaf area and combine it with Total leaf area
    reDF[reDF$Variable=="LAI","Variable"] <- "Leaf area"
    reDF$Variable[reDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf area")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_area"] <- res$b
    intDF$se[intDF$variable=="leaf_area"] <- res$se
    intDF$p_value[intDF$variable=="leaf_area"] <- res$pval
    intDF$ns[intDF$variable=="leaf_area"] <- ns
    intDF$ne[intDF$variable=="leaf_area"] <- l
    intDF$ci_lb[intDF$variable=="leaf_area"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_area"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="LMA")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="LMA"] <- res$b
    intDF$se[intDF$variable=="LMA"] <- res$se
    intDF$p_value[intDF$variable=="LMA"] <- res$pval
    intDF$ns[intDF$variable=="LMA"] <- ns
    intDF$ne[intDF$variable=="LMA"] <- l
    intDF$ci_lb[intDF$variable=="LMA"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="LMA"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total root length")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="Root_length"] <- res$b
    intDF$se[intDF$variable=="Root_length"] <- res$se
    intDF$p_value[intDF$variable=="Root_length"] <- res$pval
    intDF$ns[intDF$variable=="Root_length"] <- ns
    intDF$ne[intDF$variable=="Root_length"] <- l
    intDF$ci_lb[intDF$variable=="Root_length"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="Root_length"] <- res$ci.ub
    
    return(intDF)

}
