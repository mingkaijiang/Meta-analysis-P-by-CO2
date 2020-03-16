scenario_illustration_plot_high_P <- function() {
    ### create 6 dataframe for 3 different scenarios
    ### considering high P as control
    ### S1: multiplicative, positive CO2 response
    ### S2: synergistic, positive CO2 response
    ### S3: antagonistic, positive CO2 response
    ### S4: multiplicative, negative CO2 response
    ### S5: synergistic, negative CO2 response
    ### S6: antagonistic, negative CO2 response
    
    ### along P and CO2 gradient
    
    ### create empty dataframes
    s1DF <- data.frame(rep(c(1:2),2), rep(c("aCO2", "eCO2"), each=2), NA)
    colnames(s1DF) <- c("P_trt", "CO2_trt", "value")
    s6DF <- s5DF <- s4DF <- s3DF <- s2DF <- s1DF
    
    ### assign values, positive CO2 effect
    s1DF$value <- c(1.0, 1.3,
                    1.15, 1.495)

    s2DF$value <- c(1.0, 1.3,
                    1.15, 1.69)

    s3DF$value <- c(1.0, 1.3, 
                    1.15, 1.365)
    
    ### assign values, positive CO2 effect
    s4DF$value <- c(1.3, 1.0,
                    1.105, 0.85)
    
    s5DF$value <- c(1.3, 1.0,
                    1.105, 0.7)
    
    s6DF$value <- c(1.3, 1.0,
                    1.105, 0.95)
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=s1DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s1DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Multiplicative effect") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = c(0.5, 0.8),
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.08, label="15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1, yend = 1.15,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.15, yend = 1.15,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=1.4, label="15 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.3, yend = 1.5,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.5, yend = 1.5,
                 colour = "black", size=1)+
        scale_color_discrete(name=expression(paste(CO[2], " treatment")),
                                 labels=c(expression(aCO[2]), expression(eCO[2])))+
        ylim(0.5, 2)
    
    
    ### plot  - elevated P effect at aC and eC
    p2 <- ggplot() +  
        geom_point(data=s2DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s2DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Synergistic effect") +
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
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.08, label="15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1, yend = 1.15,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.15, yend = 1.15,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=1.5, label="30 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.3, yend = 1.69,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.69, yend = 1.69,
                 colour = "black", size=1)+
        ylim(0.5, 2.0)
    
    ### plot  - elevated P effect at aC and eC
    p3 <- ggplot() +  
        geom_point(data=s3DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s3DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Antagonistic effect") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.08, label="15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1, yend = 1.15,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1, yend = 1,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.15, yend = 1.15,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=1.335, label="5 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.3, yend = 1.365,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.365, yend = 1.365,
                 colour = "black", size=1)+
        ylim(0.5, 2.0)
    
    
    
    p4 <- ggplot() +  
        geom_point(data=s4DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s4DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Multiplicative effect") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.2, label="-15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1.3, yend = 1.105,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.105, yend = 1.105,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=0.925, label="-15 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.0, yend = 0.85,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 0.85, yend = 0.85,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.0, yend = 1.0,
                 colour = "black", size=1)+
        scale_color_discrete(name=expression(paste(CO[2], " treatment")),
                             labels=c(expression(aCO[2]), expression(eCO[2])))+
        ylim(0.5, 2)
    
    ### plot  - elevated P effect at aC and eC
    p5 <- ggplot() +  
        geom_point(data=s5DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s5DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Synergistic effect") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.2, label="-15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1.3, yend = 1.105,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.105, yend = 1.105,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=0.85, label="-30 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.0, yend = 0.7,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.0, yend = 1.0,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 0.7, yend = 0.7,
                 colour = "black", size=1)+
        ylim(0.5, 2.0)
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=s6DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s6DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Antagonistic effect") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_continuous(limit=c(0.5, 2.5),
                           breaks=c(1,2),
                           labels=c("1" = "hP", "2" = "lP"))+
        annotate("text", x=0.6, y=1.2, label="-15 %", size=5)+
        annotate("segment", x = 0.9, xend = 0.9, y = 1.3, yend = 1.105,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.3, yend = 1.3,
                 colour = "black", size=1)+
        annotate("segment", x = 0.87, xend = 0.93, y = 1.105, yend = 1.105,
                 colour = "black", size=1)+
        annotate("text", x=2.4, y=0.975, label="-5 %", size=5)+
        annotate("segment", x = 2.1, xend = 2.1, y = 1.0, yend = 0.95,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 1.0, yend = 1.0,
                 colour = "black", size=1)+
        annotate("segment", x = 2.07, xend = 2.13, y = 0.95, yend = 0.95,
                 colour = "black", size=1)+
        ylim(0.5, 2.0)
    

    ### Plotting
    pdf("output/scenario_illustration.pdf", height=10, width=8)
    plot_grid(p1, p4, p2, p5, p3, p6,
              labels="AUTO", ncol=2, align="v", axis = "l")
    dev.off()
}