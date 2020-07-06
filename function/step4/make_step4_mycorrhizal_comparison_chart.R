make_step4_mycorrhizal_comparison_chart <- function(inDF) {
    
    inDF$Mycorrhizae_2 <- as.factor(inDF$Mycorrhizae_2)
    
    ### perform statistics  
    subDF1 <- subset(inDF, Variable == "Aboveground biomass")
    
    ### split into ECM and AM
    subDF2_1 <- subset(subDF1, Mycorrhizae_2 == "ECM")
    subDF2_2 <- subset(subDF1, Mycorrhizae_2 == "AM")
    
    ### subset AM and split into woody and nonwoody
    subDF3_1 <- subset(subDF2_2, Vegetation_type=="Woody")
    subDF3_2 <- subset(subDF2_2, Vegetation_type=="Nonwoody")
    
    ### subset woody and split into ECM and AM
    subDF4 <- subset(subDF1, Vegetation_type=="Woody")
    subDF4_1 <- subset(subDF4, Mycorrhizae_2 == "ECM")
    subDF4_2 <- subset(subDF4, Mycorrhizae_2 == "AM")
    
    ### prepare storage unit
    sumDF1 <- data.frame(c("AM", "ECM"), NA, NA, NA, NA, NA, NA, NA)
    colnames(sumDF1) <- c("Myco", "interaction", 
                          "ns", "ne", "se", "p_value", "ci_lb", "ci_ub")
    
    sumDF2 <- data.frame(c("Woody", "Nonwoody"), NA, NA, NA, NA, NA, NA, NA)
    colnames(sumDF2) <- c("PFT", "interaction", 
                         "ns", "ne", "se", "p_value", "ci_lb", "ci_ub")
    
    sumDF3 <- sumDF1
    
    ### statistics - model 1
    res1 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = subDF2_1)
    
    l1 <- length(subDF2_1$Literature)
    ns1 <- length(unique(subDF2_1$Literature))

    ### statistics - model 2
    res2 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                   random = ~1 | random_factor, data = subDF2_2)
    
    l2 <- length(subDF2_2$Literature)
    ns2 <- length(unique(subDF2_2$Literature))
    
    ### assign
    sumDF1$ns[sumDF1$Myco=="ECM"] <- ns1
    sumDF1$ne[sumDF1$Myco=="ECM"] <- l1
    
    sumDF1$ns[sumDF1$Myco=="AM"] <- ns2
    sumDF1$ne[sumDF1$Myco=="AM"] <- l2
    
    
    ### predicted models 1 and 2
    predDF1 <- predict(res1, newmods = 0.2, 
                      addx=T) 
    
    predDF2 <- predict(res2, newmods = 0.2, 
                       addx=T) 
    
    ### assign
    sumDF1$interaction[sumDF1$Myco=="ECM"] <- predDF1$pred
    sumDF1$se[sumDF1$Myco=="ECM"] <- predDF1$se
    sumDF1$ci_lb[sumDF1$Myco=="ECM"] <- predDF1$ci.lb
    sumDF1$ci_ub[sumDF1$Myco=="ECM"] <- predDF1$ci.ub
    
    sumDF1$interaction[sumDF1$Myco=="AM"] <- predDF2$pred
    sumDF1$se[sumDF1$Myco=="AM"] <- predDF2$se
    sumDF1$ci_lb[sumDF1$Myco=="AM"] <- predDF2$ci.lb
    sumDF1$ci_ub[sumDF1$Myco=="AM"] <- predDF2$ci.ub
    
    
    
    ### statistics - model 3
    res1 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                   random = ~1 | random_factor, data = subDF3_1)
    
    l1 <- length(subDF3_1$Literature)
    ns1 <- length(unique(subDF3_1$Literature))
    
    ### statistics - model 4
    res2 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                   random = ~1 | random_factor, data = subDF3_2)
    
    l2 <- length(subDF3_2$Literature)
    ns2 <- length(unique(subDF3_2$Literature))
    
    ### assign
    sumDF2$ns[sumDF2$PFT=="Woody"] <- ns1
    sumDF2$ne[sumDF2$PFT=="Woody"] <- l1

    sumDF2$ns[sumDF2$PFT=="Nonwoody"] <- ns2
    sumDF2$ne[sumDF2$PFT=="Nonwoody"] <- l2
    
    
    ### predicted models 3 and 4
    predDF1 <- predict(res1, newmods = 0.2, 
                       addx=T) 
    
    predDF2 <- predict(res2, newmods = 0.2, 
                       addx=T) 
    
    ### assign
    sumDF2$interaction[sumDF2$PFT=="Woody"] <- predDF1$pred
    sumDF2$se[sumDF2$PFT=="Woody"] <- predDF1$se
    sumDF2$ci_lb[sumDF2$PFT=="Woody"] <- predDF1$ci.lb
    sumDF2$ci_ub[sumDF2$PFT=="Woody"] <- predDF1$ci.ub
    
    sumDF2$interaction[sumDF2$PFT=="Nonwoody"] <- predDF2$pred
    sumDF2$se[sumDF2$PFT=="Nonwoody"] <- predDF2$se
    sumDF2$ci_lb[sumDF2$PFT=="Nonwoody"] <- predDF2$ci.lb
    sumDF2$ci_ub[sumDF2$PFT=="Nonwoody"] <- predDF2$ci.ub
    
    
    ### statistics - model 5
    res1 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                   random = ~1 | random_factor, data = subDF4_1)
    
    l1 <- length(subDF4_1$Literature)
    ns1 <- length(unique(subDF4_1$Literature))
    
    ### statistics - model 6
    res2 <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                   random = ~1 | random_factor, data = subDF4_2)
    
    l2 <- length(subDF4_2$Literature)
    ns2 <- length(unique(subDF4_2$Literature))
    
    ### assign
    sumDF3$ns[sumDF3$Myco=="ECM"] <- ns1
    sumDF3$ne[sumDF3$Myco=="ECM"] <- l1
    
    sumDF3$ns[sumDF3$Myco=="AM"] <- ns2
    sumDF3$ne[sumDF3$Myco=="AM"] <- l2
    
    
    ### predicted models 5 and 6
    predDF1 <- predict(res1, newmods = 0.2, 
                       addx=T) 
    
    predDF2 <- predict(res2, newmods = 0.2, 
                       addx=T) 
    
    ### assign
    sumDF3$interaction[sumDF3$Myco=="ECM"] <- predDF1$pred
    sumDF3$se[sumDF3$Myco=="ECM"] <- predDF1$se
    sumDF3$ci_lb[sumDF3$Myco=="ECM"] <- predDF1$ci.lb
    sumDF3$ci_ub[sumDF3$Myco=="ECM"] <- predDF1$ci.ub
    
    sumDF3$interaction[sumDF3$Myco=="AM"] <- predDF2$pred
    sumDF3$se[sumDF3$Myco=="AM"] <- predDF2$se
    sumDF3$ci_lb[sumDF3$Myco=="AM"] <- predDF2$ci.lb
    sumDF3$ci_ub[sumDF3$Myco=="AM"] <- predDF2$ci.ub
    
    
    ### calculate range for plotting purposes
    sumDF1$pos <- (exp(sumDF1$ci_ub) - 1) * 100
    sumDF1$neg <- (exp(sumDF1$ci_lb) - 1) * 100
    sumDF1$interaction <- (exp(sumDF1$interaction) - 1) * 100
    
    sumDF2$pos <- (exp(sumDF2$ci_ub) - 1) * 100
    sumDF2$neg <- (exp(sumDF2$ci_lb) - 1) * 100
    sumDF2$interaction <- (exp(sumDF2$interaction) - 1) * 100
    
    sumDF3$pos <- (exp(sumDF3$ci_ub) - 1) * 100
    sumDF3$neg <- (exp(sumDF3$ci_lb) - 1) * 100
    sumDF3$interaction <- (exp(sumDF3$interaction) - 1) * 100
    
    
    ### plotting script
    p1a <- ggplot(sumDF1)+ 
        geom_hline(yintercept = 0.0)+
        geom_errorbar(aes(x=Myco, ymin=neg, ymax=pos, color=factor(Myco)), width=0.2) + 
        geom_point(aes(x=Myco, y=interaction, fill=Myco), 
                   size=8, shape=21)+
        labs(x="", y=expression("LP effect on " * eCO[2] * " response (%)"))+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "bottom",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-60, 0))+
        scale_color_manual(name=paste("Mycorrhizae group"),
                           limits=c("AM", "ECM"),
                           values=c("black", "grey"),
                           labels=c("AM", "ECM"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Mycorrhizae group"),
                          limits=c("AM", "ECM"),
                          values=c("black", "grey"),
                          labels=c("AM", "ECM"))+        
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    p1b <- ggplot(sumDF2)+ 
        geom_hline(yintercept = 0.0)+
        geom_errorbar(aes(x=PFT, ymin=neg, ymax=pos, color=factor(PFT)), width=0.2) + 
        geom_point(aes(x=PFT, y=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "bottom",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-60, 0))+
        scale_color_manual(name=paste("AM plants"),
                           limits=c("Woody", "Nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("AM plants"),
                          limits=c("Woody", "Nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(sumDF3)+ 
        geom_hline(yintercept = 0.0)+
        geom_errorbar(aes(x=Myco, ymin=neg, ymax=pos, color=factor(Myco)), width=0.2) + 
        geom_point(aes(x=Myco, y=interaction, fill=Myco), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "bottom",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-60, 0))+
        scale_color_manual(name=paste("Woody plants"),
                           limits=c("AM", "ECM"),
                           values=c("black", "grey"),
                           labels=c("AM", "ECM"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Woody plants"),
                          limits=c("AM", "ECM"),
                          values=c("black", "grey"),
                          labels=c("AM", "ECM"))+        
        ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/step4/Figure_9_mycorrhizae_comparison.pdf", 
        width=12, height=6)
    plot_grid(p1a, p1b, p1c, 
              rel_widths=c(1.05,1,1),
              labels=c(""), ncol=3, align="h", axis = "l")    
    dev.off()
    
    
}

