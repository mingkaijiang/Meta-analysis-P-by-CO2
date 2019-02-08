###This script plays around with metafor package to understand it
metafor_explore <- function(inDF) {
    
    if(!dir.exists("output/forest_plot_biomass")) {
        dir.create("output/forest_plot_biomass", showWarnings = FALSE)
    }
    
    ### Example
    #library("metafor")
    #data("dat.bcg", package = "metafor")
    #print(dat.bcg, row.names = FALSE)
    #
    #dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos,
    #               di = cneg, data = dat.bcg, append = TRUE)
    #
    #print(dat[,-c(4:7)], row.names = FALSE)
    #
    #print(dat, row.names = FALSE)
    
    ###  testing outcome and variance calculation using metafor package 
    tDF <- subset(inDF, Variable=="Leaf biomass")
    test <- escalc(measure = "RR", ai = aCaP_mean, bi = aCeP_mean, ci = eCaP_mean,
                   di = eCeP_mean, data = tDF, append = TRUE)
    
    ### random-effect model
    res <- rma(yi, vi, data = test)
    
    ### back transform the result 
    test$response_mean <- exp(test$yi)
    test$response_variance <- exp(test$vi)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### forest plot
    pdf("output/forest_plot_biomass/leaf_biomass_response_ratio.pdf",
        height=12, width=9)
    forest(res, slab = test$Literature,
           xlim = c(-16, 6), at = log(c(0.05, 0.25, 1, 4)), atransf = exp)
           #ilab = cbind(test$aCaP_mean, test$aCeP_mean, test$eCaP_mean, test$eCeP_mean),
           #ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.75)
    #op <- par(cex = 0.75, font = 2)
    #text(c(-9.5, -8, -6, -4.5), 15, c("aP", "eP", "aP", "eP"))
    #text(c(-8.75, -5.25), 16, c(expression(aCO[2]), expression(eCO[2])))
    #text(-16, 15, "Author(s) and Year", pos = 4)
    #text(6, 15, "Relative Response [95% CI]", pos = 2)
    #par(op)
    dev.off()
    
    ### mixed effect model
    res <- rma(yi, vi, mods = cbind(Trt_eC_by_aC, Trt_eP_by_aP, Trt_aCO2), data = test)
    
    ## categorical factor
    res <- rma(yi, vi, mods = ~ factor(Vegetation_type) + Trt_eC_by_aC + Trt_eP_by_aP, data = test, knha=T)
    
    ### check for type I error
    pval1 <- pval2 <- rep(NA, 10000)
    for (i in 1:10000) {
        xi <- rnorm(57)
        pval1[i] <- rma(yi, vi, mods = xi, data = test, method = "DL")$pval[2]
        pval2[i] <- rma(yi, vi, mods = xi, data = test, method = "DL",
                        knha = TRUE)$pval[2]
        }
    mean(pval1 < 0.05)
    mean(pval2 < 0.05)
    
    
    ## check for influential studies
    inf <- influence(res)
    plot(inf, plotdfb = TRUE)
    
    ### exclude these studies to reobtain result
    test2 <- test[-c(5,6,20,42,44,47),]
    res <- rma(yi, vi, mods = ~ factor(Vegetation_type) + Trt_eC_by_aC + Trt_eP_by_aP, data = test2, knha=T,
               control=list(stepadj=0.5))
    res    
    
    forest(test2$yi, test2$vi, atransf = exp, #ylim = c(-3.5, 16),
           at = log(c(0.05, 0.25, 1, 4, 20)), #xlim = c(-9, 7),
           slab = paste(test2$Literature))
    res2<- rma(yi, vi, mods = ~ factor(Vegetation_type) -1, knha=T, data = test2)
    preds <- predict(res)
    addpoly(preds$pred, sei = preds$se, atransf = exp)
    funnel(res)
    qqnorm(res)
    
}