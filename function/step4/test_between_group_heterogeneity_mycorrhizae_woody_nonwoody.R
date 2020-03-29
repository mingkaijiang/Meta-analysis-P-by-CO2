test_step4_between_group_heterogeneity_mycorrhizae_woody_nonwoody <- function(subDF) {
    
    ### 
    inDF <- subDF[subDF$Mycorrhizae_2=="AM",]

    var.list <- c("Aboveground biomass", "Belowground biomass", 
                  "Total plant biomass", "Plant biomass")
    
    ### generate a output dataframe
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "nwd_ne", "wd_ne")
    
    
    
    ####### Effect of LP on eCO2 response
    var.list1 <- c("Aboveground biomass", 
                   "Total plant biomass")
        
    for (i in var.list1) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, v_variance >= 0.001)
        
        
        res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type), data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
            
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$nwd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        sumDF$wd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        
        
    }
    
    ### root and belowground biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, v_variance >= 0.001)
    
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
               mods = ~factor(Vegetation_type), data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Belowground biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Belowground biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Belowground biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Belowground biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Belowground biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Belowground biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Belowground biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Belowground biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Belowground biomass"] <- res$ci.ub[2]
    sumDF$nwd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    
    ### all biomass
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", 
                                         "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, v_variance >= 0.001)
    
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type), data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$nwd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    write.csv(sumDF, "output/step4/statistics_lp_effect_on_CO2_response_am.csv", row.names=F)
    
    ####### CO2 effect under LP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "nwd_ne", "wd_ne")
    
    var.list2 <- c("Aboveground biomass", 
                   "Total plant biomass")
    
    for (i in var.list2) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
        
        res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type), 
                   data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
        
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$nwd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        sumDF$wd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        
        
    }
    
    ### root biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type), 
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
    sumDF$nwd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    ### all biomass
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type), 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$nwd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    write.csv(sumDF, "output/step4/statistics_CO2_effect_under_LP_am.csv", row.names=F)
    
    
    
    ####### CO2 effect under HP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "nwd_ne", "wd_ne")
    
    var.list3 <- c("Aboveground biomass", 
                   "Total plant biomass")
    
    
    for (i in var.list3) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
        
        res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                      mods = ~factor(Vegetation_type), 
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
        sumDF$nwd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
        sumDF$wd_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
        
        
    }
    

    ### root biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type), 
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
    sumDF$nwd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    ### all plant
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", 
                                         "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                  mods = ~factor(Vegetation_type), 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$nwd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Nonwoody"])
    sumDF$wd_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Vegetation_type=="Woody"])
    
    
    write.csv(sumDF, "output/step4/statistics_CO2_effect_under_HP_am.csv", row.names=F)
    
    
    
    
    ### 
    inDF <- subDF[subDF$Vegetation_type=="Woody",]

    var.list <- c("Aboveground biomass", "Belowground biomass", 
                  "Total plant biomass", "Plant biomass")
    
    ### generate a output dataframe
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "ecm_ne", "am_ne")
    
    
    
    ####### Effect of LP on eCO2 response
    var.list1 <- c("Aboveground biomass", 
                   "Total plant biomass")
    
    for (i in var.list1) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, v_variance >= 0.001)
        
        
        res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                      mods = ~factor(Mycorrhizae_2), data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
        
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$ecm_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
        sumDF$am_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
        
        
    }
    
    ### root and belowground biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, v_variance >= 0.001)
    
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Belowground biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Belowground biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Belowground biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Belowground biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Belowground biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Belowground biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Belowground biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Belowground biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Belowground biomass"] <- res$ci.ub[2]
    sumDF$ecm_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    ### total biomass
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", 
                                         "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, v_variance >= 0.001)
    
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$ecm_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    write.csv(sumDF, "output/step4/statistics_lp_effect_on_CO2_response_woody.csv", row.names=F)
    
    ####### CO2 effect under LP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "ecm_ne", "am_ne")
    
    var.list2 <- c("Aboveground biomass", 
                   "Total plant biomass")
    
    for (i in var.list2) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
        
        res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                      mods = ~factor(Mycorrhizae_2), 
                      data = subDF1)
        
        sumDF$Qm[sumDF$variable == i] <- res$QM
        sumDF$Qe[sumDF$variable == i] <- res$QE
        
        sumDF$Qm_pval[sumDF$variable == i] <- res$QMp
        sumDF$Qe_pval[sumDF$variable == i] <- res$QEp
        
        sumDF$estimate[sumDF$variable == i] <- res$beta[2]
        sumDF$se[sumDF$variable == i] <- res$se[2]
        sumDF$zval[sumDF$variable == i] <- res$zval[2]
        sumDF$ci.lb[sumDF$variable == i] <- res$ci.lb[2]
        sumDF$ci.ub[sumDF$variable == i] <- res$ci.ub[2]
        sumDF$ecm_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
        sumDF$am_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
        
        
    }
    
    ### root biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), 
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
    sumDF$ecm_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    ### total biomass
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", 
                                         "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_aP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$ecm_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    write.csv(sumDF, "output/step4/statistics_CO2_effect_under_LP_woody.csv", row.names=F)
    
    
    
    ####### CO2 effect under HP treatment
    sumDF <- data.frame(var.list, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA, NA)
    
    colnames(sumDF) <- c("variable", "Qm", "Qm_pval", 
                         "Qe", "Qe_pval", 
                         "estimate", "se", "zval", "ci.lb", "ci.ub", "ecm_ne", "am_ne")
    
    var.list3 <- c("Aboveground biomass", 
                   "Total plant biomass")
    
    
    for (i in var.list3) {
        subDF1 <- subset(inDF, Variable == i)
        #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
        
        res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                      mods = ~factor(Mycorrhizae_2), 
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
        sumDF$ecm_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
        sumDF$am_ne[sumDF$variable == i] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
        
        
    }
    
    
    ### root biomass
    subDF1 <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), 
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
    sumDF$ecm_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Belowground biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    ### total 
    subDF1 <- subset(inDF, Variable%in%c("Aboveground biomass", 
                                         "Total plant biomass",
                                         "Root biomass", "Belowground biomass"))
    #subDF1 <- subset(subDF1, variance_co2_eP >= 0.001)
    
    res <- rma.mv(log_co2_aP, variance_co2_eP, random = ~1 | random_factor,
                  mods = ~factor(Mycorrhizae_2), 
                  data = subDF1)
    
    sumDF$Qm[sumDF$variable == "Plant biomass"] <- res$QM
    sumDF$Qe[sumDF$variable == "Plant biomass"] <- res$QE
    
    sumDF$Qm_pval[sumDF$variable == "Plant biomass"] <- res$QMp
    sumDF$Qe_pval[sumDF$variable == "Plant biomass"] <- res$QEp
    
    sumDF$estimate[sumDF$variable == "Plant biomass"] <- res$beta[2]
    sumDF$se[sumDF$variable == "Plant biomass"] <- res$se[2]
    sumDF$zval[sumDF$variable == "Plant biomass"] <- res$zval[2]
    sumDF$ci.lb[sumDF$variable == "Plant biomass"] <- res$ci.lb[2]
    sumDF$ci.ub[sumDF$variable == "Plant biomass"] <- res$ci.ub[2]
    sumDF$ecm_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="ECM"])
    sumDF$am_ne[sumDF$variable == "Plant biomass"] <- length(subDF1$Literature[subDF1$Mycorrhizae_2=="AM"])
    
    
    write.csv(sumDF, "output/step4/statistics_CO2_effect_under_HP_woody.csv", row.names=F)
    
}