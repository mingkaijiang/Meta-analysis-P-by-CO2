make_step2_metafor_statistics_advanced <- function(inDF, intDF) {
  
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Aboveground biomass")
    tDF <- subset(tDF, v_variance >= 0.001)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="aboveground_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="aboveground_biomass")
    
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="aboveground_biomass")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="aboveground_biomass"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="aboveground_biomass"] <- pub_bias_test$pval
    
    #test2 <- permutest(res2)
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf biomass")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_biomass")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_biomass")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_biomass"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_biomass"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem biomass")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_biomass")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_biomass")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_biomass"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_biomass"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(inDF, Variable=="Root biomass")
    tDF <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    
    tDF <- subset(tDF, v_variance >= 0.001)
    #tDF <- subset(tDF, v_variance <= 2)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_biomass") 
    
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_biomass")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_biomass")
    
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_biomass"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_biomass"] <- pub_bias_test$pval
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, v_variance >= 0.001)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_biomass")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_biomass")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_biomass"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_biomass"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, digits=5, 
    #           control=list(stepadj=0.05))

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_N_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF,digits=5, control=list(stepadj=0.5))
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_N_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_N_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_N_content"] <- pub_bias_test$pval

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_P_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_P_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_P_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_P_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_N_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_N_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_N_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_N_content"] <- pub_bias_test$pval
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_P_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_P_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_P_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_P_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_N_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_N_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_N_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_N_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_P_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_P_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_P_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_P_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_N_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_N_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_N_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_N_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_P_content")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_P_content")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_P_content"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_P_content"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Rang
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_N_concentration")
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_N_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_N_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_N_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_N_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_P_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF,digits=5, control=list(stepadj=0.5))
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_P_concentration")
    
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_P_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_P_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_P_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_P_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_P_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_P_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)

    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_N_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_N_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_N_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_N_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_P_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_P_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_P_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_P_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_N_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_N_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_N_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_N_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_N_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_N_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_N_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_N_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_P_concentration")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_P_concentration")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_P_concentration"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_P_concentration"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="CO2 assimilation rate")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance,  
    #              random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="CO2_assimilation_rate") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="CO2_assimilation_rate")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="CO2_assimilation_rate")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="CO2_assimilation_rate"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="CO2_assimilation_rate"] <- pub_bias_test$pval
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stomatal conductance")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stomatal_conductance") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stomatal_conductance")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stomatal_conductance")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stomatal_conductance"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stomatal_conductance"] <- pub_bias_test$pval
    
    
    ### change LAI to leaf area and combine it with Total leaf area
    inDF[inDF$Variable=="LAI","Variable"] <- "Leaf area"
    inDF$Variable[inDF$Variable=="Total leaf area"] <- "Leaf area"
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf area")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_area") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_area")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_area")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_area"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_area"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="LMA")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="LMA") 
    
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="LMA")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF,digits=5, control=list(stepadj=0.5))
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="LMA")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="LMA"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="LMA"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="SLA")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="SLA") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="SLA")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="SLA")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="SLA"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="SLA"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total root length")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="Root_length") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="Root_length")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="Root_length")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="Root_length"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="Root_length"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_NP") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_NP")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF, digits=5,method="EB", control=list(stepadj=0.5,threshold=1e-8))
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="leaf_NP")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="leaf_NP"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="leaf_NP"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_NP") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_NP")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="stem_NP")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="stem_NP"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="stem_NP"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_NP") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_NP")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="root_NP")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="root_NP"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="root_NP"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_NP") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_NP")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="total_NP")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="total_NP"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="total_NP"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    tDF$v_variance <- 1/tDF$Sample.Size
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="N_uptake") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="N_uptake")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="N_uptake")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="N_uptake"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="N_uptake"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug P root-1", "mg P g-1 root", "ug P mg root-1"))
    tDF$v_variance <- 1/tDF$Sample.Size
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="P_uptake") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="P_uptake")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="P_uptake")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="P_uptake"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="P_uptake"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="WUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="WUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="WUE")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF,digits=5, control=list(stepadj=0.5))
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="WUE")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="WUE"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="WUE"] <- pub_bias_test$pval
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="NUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="NUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="NUE")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="NUE")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="NUE"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="NUE"] <- pub_bias_test$pval
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="PUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### find outliters and remove from the data
    Q <- quantile(tDF$log_interaction, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(tDF$log_interaction)
    
    up <-  Q[2]+1.5*iqr # Upper Range  
    low<- Q[1]-1.5*iqr # Lower Range
    
    #tDF <- tDF[tDF$log_interaction <= up & tDF$log_interaction >= low, ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="PUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="PUE")
    
    ### leave one out analysis, based on simple model
    res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    looDF <- leave1out(res2)
    make_step2_interaction_leave_one_out_plot(res, res2, looDF, var.name="PUE")
    
    ### test for funnel plot assymetry to show possible publication bias, based on simple model
    pub_bias_test <- regtest(res2)
    intDF$z_score[intDF$variable=="PUE"] <- pub_bias_test$zval
    intDF$z_score_p_value[intDF$variable=="PUE"] <- pub_bias_test$pval
    
    
    return(intDF)

}
