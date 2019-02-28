scenario_illustration_plot <- function() {
    ### create three dataframe for 3 different scenarios
    ### S1: multiplicative
    ### S2: multiplicative, synergistic
    ### S3: multiplicative, antagonistic
    ### along P and CO2 gradient
    
    ### create empty dataframes
    s1DF <- data.frame(rep(c(1:5),2), rep(c("aCO2", "eCO2"), each=5), NA)
    colnames(s1DF) <- c("P_trt", "CO2_trt", "value")
    s3DF <- s2DF <- s1DF
    
    ### assign values
    s1DF$value <- c(1.0, 1.3, 1.69, 2.2, 2.86,
                   1.15, 1.5, 1.94, 2.53, 3.28)

    s2DF$value <- c(1.0, 1.3, 1.82, 2.73, 4.37,
                    1.15, 1.56, 2.28, 3.55, 5.9)

    s3DF$value <- c(1.0, 1.3, 1.56, 1.72, 1.72,
                    1.15, 1.43, 1.64, 1.72, 1.63)

    ### make a subset
    s1DF.sub <- subset(s1DF, P_trt%in%c(1,2))
    s2DF.sub <- subset(s2DF, P_trt%in%c(1,2))
    s3DF.sub <- subset(s3DF, P_trt%in%c(1,2))
    
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=s1DF, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s1DF, 
                  aes(P_trt, value, col=CO2_trt), lty=2, lwd=1.5)+
        geom_point(data=s1DF.sub, 
                   aes(P_trt, value, col=CO2_trt),
                   shape=19, size=5)+
        geom_line(data=s1DF.sub, 
                  aes(P_trt, value, col=CO2_trt), lwd=1.5)+
        xlab("Phosphorus treatment") + 
        ylab("Response") +
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
        scale_x_continuous(labels=c("1" = "aP", "2" = "eP1",
                                  "3" = "eP2", "4" = "eP3",
                                  "5" = "eP4"))
    
    plot(p1)
    
    
    
    
    ### Plotting
    pdf("output/scenario_illustration.pdf")
    plot_grid(p1, p2, p3,
              labels="AUTO", ncol=1, align="h", axis = "l")
    dev.off()
}