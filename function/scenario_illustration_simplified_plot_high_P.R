scenario_illustration_simplified_plot_high_P <- function() {
    ### create 3 dataframe for 9 different scenarios
    ### considering high P as control
    
    ### along P and CO2 gradient
    
    ### create empty dataframes
    s1DF <- data.frame(rep(c(1:2),2), rep(c("aCO2", "eCO2"), each=2), NA)
    colnames(s1DF) <- c("P_trt", "CO2_trt", "value")
    s3DF <- s2DF <- s1DF
    
    ### assign values, positive CO2 effect
    s1DF$value <- c(1.0, 1.3,
                    1.2, 1.56)

    s2DF$value <- c(1.8, 1.26,
                    1.44, 0.792)
    
    s3DF$value <- c(1.4, 1.82,
                    1.12, 1.456)
    
    ### plot  
    p1 <- ggplot() +  
        geom_point(data=s1DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        xlab("Phosphorus treatment") + 
        ylab("Variable Value") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = c(0.3, 0.8),
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.8, 3),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        ### co2 effect under hP
        annotate("text", x=0.85, y=1.1, label="+20 %", size=5)+
        annotate("segment", x = 1, xend = 1, y = 1, yend = 1.2,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1.2, yend = 1.2,
                 colour = "black", size=1)+
        ### lP effect under aCO2
        annotate("text", x=1.85, y=1.1, label="+30 %", size=5)+
        annotate("segment", x = 2, xend = 2, y = 1, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 1.97, xend = 2.03, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 1.97, xend = 2.03, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 1, xend = 2, y = 1, yend = 1.0,
                 colour = "black", size=1, lty=2)+
        ### multiplicative effect
        annotate("text", x=2.35, y=1.28, label="+56 %", size=5)+
        annotate("segment", x = 2.2, xend = 2.2, y = 1, yend = 1.56,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 1.56, yend = 1.56,
                 colour = "black", size=1)+
        ### synergistic effect
        annotate("text", x=2.7, y=1.8, label="Synergistic", size=5)+
        annotate("segment", x = 2.3, xend = 3.0, y = 1.56, yend = 1.56,
                 colour = "black", size=1)+
        annotate("segment", x = 2.65, xend = 2.65, y = 1.56, yend = 1.7,
                 colour = "black", size=1, arrow=arrow())+
        ### antagonistic effect
        annotate("text", x=2.7, y=1.3, label="Antagonistic", size=5)+
        annotate("segment", x = 2.65, xend = 2.65, y = 1.56, yend = 1.4,
                 colour = "black", size=1, arrow=arrow())+
        scale_color_discrete(name=expression(paste(CO[2], " treatment")),
                                 labels=c(expression(aCO[2]), expression(eCO[2])))+
        ylim(0.5, 2)
    
    
    
    ### plot  
    p2 <- ggplot() +  
        geom_point(data=s2DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        xlab("Phosphorus treatment") + 
        ylab("Variable Value") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.8, 3),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        ### co2 effect under hP
        annotate("text", x=0.85, y=1.62, label="-20 %", size=5)+
        annotate("segment", x = 1, xend = 1, y = 1.8, yend = 1.44,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1.8, yend = 1.8,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1.44, yend = 1.44,
                 colour = "black", size=1)+
        ### lP effect under aCO2
        annotate("text", x=1.85, y=1.5, label="-30 %", size=5)+
        annotate("segment", x = 2, xend = 2, y = 1.8, yend = 1.26,
                 colour = "black", size=1)+
        annotate("segment", x = 1.97, xend = 2.03, y = 1.8, yend = 1.8,
                 colour = "black", size=1)+
        annotate("segment", x = 1.97, xend = 2.03, y = 1.26, yend = 1.26,
                 colour = "black", size=1)+
        annotate("segment", x = 1, xend = 2, y = 1.8, yend = 1.8,
                 colour = "black", size=1, lty=2)+
        ### multiplicative effect
        annotate("text", x=2.35, y=1.3, label="-56 %", size=5)+
        annotate("segment", x = 2.2, xend = 2.2, y = 0.792, yend = 1.8,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 0.792, yend = 0.792,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 1.8, yend = 1.8,
                 colour = "black", size=1)+
        ### synergistic effect
        annotate("text", x=2.7, y=0.6, label="Synergistic", size=5)+
        annotate("segment", x = 2.3, xend = 3.0, y = 0.792, yend = 0.792,
                 colour = "black", size=1)+
        annotate("segment", x = 2.65, xend = 2.65, y = 0.792, yend = 0.65,
                 colour = "black", size=1, arrow=arrow())+
        ### antagonistic effect
        annotate("text", x=2.7, y=1.05, label="Antagonistic", size=5)+
        annotate("segment", x = 2.65, xend = 2.65, y = 0.792, yend = 0.95,
                 colour = "black", size=1, arrow=arrow())+
        scale_color_discrete(name=expression(paste(CO[2], " treatment")),
                             labels=c(expression(aCO[2]), expression(eCO[2])))+
        ylim(0.5, 2)
    
    ### plot  
    p3 <- ggplot() +  
        geom_point(data=s3DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        xlab("Phosphorus treatment") + 
        ylab("Variable Value") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.8, 3),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        ### co2 effect under hP
        annotate("text", x=0.85, y=1.26, label="-20 %", size=5)+
        annotate("segment", x = 1, xend = 1, y = 1.4, yend = 1.12,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1.12, yend = 1.12,
                 colour = "black", size=1)+
        annotate("segment", x = 0.97, xend = 1.03, y = 1.4, yend = 1.4,
                 colour = "black", size=1)+
        ### lP effect under aCO2
        annotate("text", x=1.75, y=1.61, label="+30 %", size=5)+
        annotate("segment", x = 1.9, xend = 1.9, y = 1.82, yend = 1.4,
                 colour = "black", size=1)+
        annotate("segment", x = 1.87, xend = 1.93, y = 1.82, yend = 1.82,
                 colour = "black", size=1)+
        annotate("segment", x = 1.87, xend = 1.93, y = 1.4, yend = 1.4,
                 colour = "black", size=1)+
        annotate("segment", x = 1, xend = 2, y = 1.4, yend = 1.4,
                 colour = "black", size=1, lty=2)+
        ### multiplicative effect
        annotate("text", x=2.35, y=1.42, label="+4 %", size=5)+
        annotate("segment", x = 2.2, xend = 2.2, y = 1.4, yend = 1.456,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 1.4, yend = 1.4,
                 colour = "black", size=1)+
        annotate("segment", x = 2.17, xend = 2.23, y = 1.456, yend = 1.456,
                 colour = "black", size=1)+
        ### synergistic effect
        annotate("text", x=2.7, y=1.66, label="Synergistic", size=5)+
        annotate("segment", x = 2.3, xend = 3.0, y = 1.456, yend = 1.456,
                 colour = "black", size=1)+
        annotate("segment", x = 2.65, xend = 2.65, y = 1.456, yend = 1.6,
                 colour = "black", size=1, arrow=arrow())+
        ### antagonistic effect
        annotate("text", x=2.7, y=1.25, label="Antagonistic", size=5)+
        annotate("segment", x = 2.65, xend = 2.65, y = 1.456, yend = 1.3,
                 colour = "black", size=1, arrow=arrow())+
        scale_color_discrete(name=expression(paste(CO[2], " treatment")),
                             labels=c(expression(aCO[2]), expression(eCO[2])))+
        ylim(0.5, 2)
    
    plot(p3)

    ### Plotting
    pdf("output/scenario_illustration.pdf", height=12, width=8)
    plot_grid(p1, p2, p3,
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
}