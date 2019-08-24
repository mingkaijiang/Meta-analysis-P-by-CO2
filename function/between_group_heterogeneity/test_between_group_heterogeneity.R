test_between_group_heterogeneity <- function(reDF100) {
    
    ### 
    inDF <- reDF100
    
    var.list <- c("Aboveground biomass", "Belowground biomass", 
                  "Total plant biomass",
                  "Leaf N concentration", "Root N concentration", 
                  "Leaf P concentration", "Root P concentration", 
                  "CO2 assimilation rate", "Leaf area", "LMA", "Total root length")
    
    ### generate a output dataframe
    sumDF <- data.frame(var.list, NA, NA, NA, #NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", #"R2", "tau2", "tau2_se", "I2", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "woody_ne", "nonwoody_ne")
    
    
    
    ####### Effect of LP on eCO2 response
    var.list1 <- c("Aboveground biomass", 
                   "Total plant biomass", 
                   "Leaf N concentration", "Root N concentration", 
                   "Leaf P concentration", "Root P concentration", 
                   "CO2 assimilation rate", "Leaf area", "LMA", "Total root length")
        
    for (i in var.list1) {
        subDF1 <- subset(inDF, Variable == i)
        subDF1 <- subset(subDF1, v_variance >= 0.01)
        
        
        res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type)+Trt_LP_HP, data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
            
        #sumDF$R2[sumDF$variable == i] <- res$R2
        #sumDF$tau2[sumDF$variable == i] <- res$tau2
        #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
        #sumDF$I2[sumDF$variable == i] <- res$I2
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$woody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        sumDF$nonwoody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        
        
    }
    
    ### root and belowground biomass
    #subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    subDF1 <- subset(inDF, Variable=="Root biomass")
    
    subDF1 <- subset(subDF1, v_variance >= 0.01)
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
               mods = ~factor(Vegetation_type)+Trt_LP_HP, data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Belowground biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Belowground biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Belowground biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Belowground biomass"] <- res$QEp
    
    #sumDF$R2[sumDF$variable == i] <- res$R2
    #sumDF$tau2[sumDF$variable == i] <- res$tau2
    #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
    #sumDF$I2[sumDF$variable == i] <- res$I2
    sumDF$estimate[sumDF$variable == "Belowground biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Belowground biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Belowground biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Belowground biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Belowground biomass"] <- res$ci.ub[2]
    sumDF$woody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    sumDF$nonwoody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    
    
    write.csv(sumDF, "output/metafor_summary_plot/statistics_lp_effect_on_CO2_response_woody_nonwoody.csv", row.names=F)
    
    
    
    ####### CO2 effect under LP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, #NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", #"R2", "tau2", "tau2_se", "I2", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "woody_ne", "nonwoody_ne")
    
    var.list2 <- c("Aboveground biomass", 
                   "Total plant biomass", 
                   "Leaf N concentration", "Root N concentration", 
                   "Leaf P concentration", "Root P concentration", 
                   "CO2 assimilation rate", "Leaf area", "Total root length")
    
    for (i in var.list2) {
        subDF1 <- subset(inDF, Variable == i)
        subDF1 <- subset(subDF1, variance_co2_aP >= 0.01)
        
        res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type)+Trt_LP_HP, 
                   data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
        
        #sumDF$R2[sumDF$variable == i] <- res$R2
        #sumDF$tau2[sumDF$variable == i] <- res$tau2
        #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
        #sumDF$I2[sumDF$variable == i] <- res$I2
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$woody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        sumDF$nonwoody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        
        
    }
    
    ### root biomass
    #subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    subDF1 <- subset(inDF, Variable=="Root biomass")
    
    subDF1 <- subset(subDF1, variance_co2_aP >= 0.01)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type)+Trt_LP_HP, 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Belowground biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Belowground biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Belowground biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Belowground biomass"] <- res$QEp
    
    #sumDF$R2[sumDF$variable == i] <- res$R2
    #sumDF$tau2[sumDF$variable == i] <- res$tau2
    #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
    #sumDF$I2[sumDF$variable == i] <- res$I2
    sumDF$estimate[sumDF$variable == "Belowground biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Belowground biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Belowground biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Belowground biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Belowground biomass"] <- res$ci.ub[2]
    sumDF$woody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    sumDF$nonwoody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    
    
    
    ### LMA
    ### Leaf P specific
    i <- "LMA"
    subDF1 <- subset(inDF, Variable == i)
    res <- rma(log_co2_eP, variance_co2_aP, 
               mods = ~factor(Vegetation_type)+Trt_LP_HP, data = subDF1)
    
    sumDF$Qm[sumDF$variable == i] <- res$QM
    sumDF$Qe[sumDF$variable == i] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
    
    #sumDF$R2[sumDF$variable == i] <- res$R2
    #sumDF$tau2[sumDF$variable == i] <- res$tau2
    #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
    #sumDF$I2[sumDF$variable == i] <- res$I2
    sumDF$estimate[sumDF$variable == i] <- res$beta[2]
    sumDF$se[sumDF$variable == i] <- res$se[2]
    sumDF$zval[sumDF$variable == i] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
    sumDF$woody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    sumDF$nonwoody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    
    
    write.csv(sumDF, "output/metafor_summary_plot/statistics_CO2_effect_under_LP_woody_nonwoody.csv", row.names=F)
    
    
    
    ####### CO2 effect under HP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, #NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", #"R2", "tau2", "tau2_se", "I2", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "woody_ne", "nonwoody_ne")
    
    var.list3 <- c("Aboveground biomass", 
                   "Total plant biomass", 
                   "Leaf N concentration", "Root N concentration", 
                   "Leaf P concentration", "Root P concentration", 
                   "CO2 assimilation rate", "LMA", "Total root length")
    
    
    for (i in var.list3) {
        subDF1 <- subset(inDF, Variable == i)
        subDF1 <- subset(subDF1, variance_co2_eP >= 0.01)
        
        res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type)+Trt_LP_HP, 
                      data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
        
        sumDF$R2[sumDF$variable == i] <- res$R2
        sumDF$tau2[sumDF$variable == i] <- res$tau2
        sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
        sumDF$I2[sumDF$variable == i] <- res$I2
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$woody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        sumDF$nonwoody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        
        
    }
    
    ### Leaf area
    i <- "Leaf area"
    subDF1 <- subset(inDF, Variable == i)
    res <- rma(log_co2_eP, variance_co2_eP, 
               mods = ~factor(Vegetation_type)+Trt_LP_HP, data = subDF1)
    
    sumDF$Qm[sumDF$variable == i] <- res$QM
    sumDF$Qe[sumDF$variable == i] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
    
    #sumDF$R2[sumDF$variable == i] <- res$R2
    #sumDF$tau2[sumDF$variable == i] <- res$tau2
    #sumDF$tau2_se[sumDF$variable == i] <- res$se.tau2
    #sumDF$I2[sumDF$variable == i] <- res$I2
    sumDF$estimate[sumDF$variable == i] <- res$beta[2]
    sumDF$se[sumDF$variable == i] <- res$se[2]
    sumDF$zval[sumDF$variable == i] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
    sumDF$woody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    sumDF$nonwoody_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    
    
    ### root biomass
    #subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    subDF1 <- subset(inDF, Variable=="Root biomass")
    
    subDF1 <- subset(subDF1, variance_co2_eP >= 0.01)
    
    res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type)+Trt_LP_HP, 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Belowground biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Belowground biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Belowground biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Belowground biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Belowground biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Belowground biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Belowground biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Belowground biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Belowground biomass"] <- res$ci.ub[2]
    sumDF$woody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    sumDF$nonwoody_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    
    
    write.csv(sumDF, "output/metafor_summary_plot/statistics_CO2_effect_under_HP_woody_nonwoody.csv", row.names=F)
    
    
}