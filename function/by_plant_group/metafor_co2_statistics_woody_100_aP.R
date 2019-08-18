metafor_co2_statistics_woody_plants_100_aP <- function(reDF, sumDF) {
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root biomass")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Aboveground biomass")
    
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Root biomass",  
                                      "Belowground biomass"))
    tDF <- subset(tDF, variance_co2_aP <= 2)
    tDF <- subset(tDF, variance_co2_aP >= 0.001)
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="belowground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF, digits=2, control=list(maxiter=1000,
    #                                                                     stepadj=0.1))

    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_N_concentration"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$variance_co2_aP > 0, ]
    
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF, control=list(stepadj=0.5))

    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_P_concentration"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$variance_co2_aP > 0, ]
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_P_concentration"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N concentration")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    sumDF$CO2_effect[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_N_concentration"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="CO2_assimilation_rate"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### change LAI to leaf area and combine it with Total leaf area
    reDF[reDF$Variable=="LAI","Variable"] <- "Leaf area"
    reDF$Variable[reDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf area")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_area"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="LMA")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="LMA"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="SLA")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="SLA"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total root length")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~ 1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- l
    
    sumDF$ci_lb[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="Root_length"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    return(sumDF)
    
}

