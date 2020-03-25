plot_leaf_N_P_concentration_comparison <- function(inDF) {
    
    ### pass in data
    #out <- inDF[inDF$Variable%in%c("Leaf N concentration", "Leaf P concentration"),]
    
    #write.csv(out, "output/step1/leaf_N_P_concentration.csv", row.names=F)
    
    
    
    
    ### read in post-processing dataset
    plotDF <- read.csv("output/step1/leaf_N_P_concentration_input.csv")
    
    
    ### prepare 1: 16 and 1:20 line
    ablines <- data.frame(seq(0.1, 10, by=0.1), NA, NA)
    colnames(ablines) <- c("x", "y1", "y2")
    ablines$y1 <- ablines$x / 16
    ablines$y2 <- ablines$x / 20
    
    
    ### aggregated arrow
    aggDF <- summaryBy(Leaf_N_aP+Leaf_N_eP+Leaf_P_aP+Leaf_P_eP~CO2_trt, data=plotDF, FUN=c(mean, sd), keep.names=T)
    n <- length(plotDF$Study[plotDF$CO2_trt=="aC"])
    
    aggDF$Leaf_N_aP.se <- aggDF$Leaf_N_aP.sd / sqrt(n)
    aggDF$Leaf_N_eP.se <- aggDF$Leaf_N_eP.sd / sqrt(n)
    
    aggDF$Leaf_P_aP.se <- aggDF$Leaf_P_aP.sd / sqrt(n)
    aggDF$Leaf_P_eP.se <- aggDF$Leaf_P_eP.sd / sqrt(n)
    
    
    leafn.aP.aC.pos <- aggDF$Leaf_N_aP.mean[aggDF$CO2_trt == "aC"] + aggDF$Leaf_N_aP.se[aggDF$CO2_trt == "aC"]
    leafn.aP.aC.neg <- aggDF$Leaf_N_aP.mean[aggDF$CO2_trt == "aC"] - aggDF$Leaf_N_aP.se[aggDF$CO2_trt == "aC"]
    
    leafn.eP.aC.pos <- aggDF$Leaf_N_eP.mean[aggDF$CO2_trt == "aC"] + aggDF$Leaf_N_eP.se[aggDF$CO2_trt == "aC"]
    leafn.eP.aC.neg <- aggDF$Leaf_N_eP.mean[aggDF$CO2_trt == "aC"] - aggDF$Leaf_N_eP.se[aggDF$CO2_trt == "aC"]
    

    leafn.aP.eC.pos <- aggDF$Leaf_N_aP.mean[aggDF$CO2_trt == "eC"] + aggDF$Leaf_N_aP.se[aggDF$CO2_trt == "eC"]
    leafn.aP.eC.neg <- aggDF$Leaf_N_aP.mean[aggDF$CO2_trt == "eC"] - aggDF$Leaf_N_aP.se[aggDF$CO2_trt == "eC"]
    
    leafn.eP.eC.pos <- aggDF$Leaf_N_eP.mean[aggDF$CO2_trt == "eC"] + aggDF$Leaf_N_eP.se[aggDF$CO2_trt == "eC"]
    leafn.eP.eC.neg <- aggDF$Leaf_N_eP.mean[aggDF$CO2_trt == "eC"] - aggDF$Leaf_N_eP.se[aggDF$CO2_trt == "eC"]
    
    
    
    leafp.aP.aC.pos <- aggDF$Leaf_P_aP.mean[aggDF$CO2_trt == "aC"] + aggDF$Leaf_P_aP.se[aggDF$CO2_trt == "aC"]
    leafp.aP.aC.neg <- aggDF$Leaf_P_aP.mean[aggDF$CO2_trt == "aC"] - aggDF$Leaf_P_aP.se[aggDF$CO2_trt == "aC"]
    
    leafp.eP.aC.pos <- aggDF$Leaf_P_eP.mean[aggDF$CO2_trt == "aC"] + aggDF$Leaf_P_eP.se[aggDF$CO2_trt == "aC"]
    leafp.eP.aC.neg <- aggDF$Leaf_P_eP.mean[aggDF$CO2_trt == "aC"] - aggDF$Leaf_P_eP.se[aggDF$CO2_trt == "aC"]
    
    
    leafp.aP.eC.pos <- aggDF$Leaf_P_aP.mean[aggDF$CO2_trt == "eC"] + aggDF$Leaf_P_aP.se[aggDF$CO2_trt == "eC"]
    leafp.aP.eC.neg <- aggDF$Leaf_P_aP.mean[aggDF$CO2_trt == "eC"] - aggDF$Leaf_P_aP.se[aggDF$CO2_trt == "eC"]
    
    leafp.eP.eC.pos <- aggDF$Leaf_P_eP.mean[aggDF$CO2_trt == "eC"] + aggDF$Leaf_P_eP.se[aggDF$CO2_trt == "eC"]
    leafp.eP.eC.neg <- aggDF$Leaf_P_eP.mean[aggDF$CO2_trt == "eC"] - aggDF$Leaf_P_eP.se[aggDF$CO2_trt == "eC"]
    
    
    ### make plot
    p1 <- ggplot()+ 
        geom_line(data=ablines, aes(x=x, y=y1), lty=2)+
        geom_line(data=ablines, aes(x=x, y=y2), lty=3)+
        annotate(geom="text", x=7, y=0.46, label="1:16 line",
                 color="black", size=10)+
        annotate(geom="text", x=7.5, y=0.32, label="1:20 line",
                 color="black", size=10)+
        geom_point(data=plotDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, color = as.factor(Vegetation_type)), 
                   size=6, shape=21, alpha=0.9)+
        geom_point(data=plotDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, fill = as.factor(Vegetation_type)), 
                   size=6, shape=21, color="black", alpha=0.9)+
        geom_line(data=plotDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, group=as.factor(Group)),
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"), alpha=0.9)+
        geom_point(data=aggDF, mapping=aes(x=Leaf_N_eP.mean, y=Leaf_P_eP.mean), 
                   size = 5, shape=22, color="black", fill="grey")+
        geom_line(data=aggDF, mapping=aes(x=Leaf_N_eP.mean, y=Leaf_P_eP.mean), size=2,
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
        geom_segment(aes(x=aggDF$Leaf_N_eP.mean[aggDF$CO2_trt=="aC"], xend = aggDF$Leaf_N_eP.mean[aggDF$CO2_trt=="aC"],
                         y=leafp.eP.aC.pos, yend=leafp.eP.aC.neg), lwd=1)+
        geom_segment(aes(x=aggDF$Leaf_N_eP.mean[aggDF$CO2_trt=="eC"], xend = aggDF$Leaf_N_eP.mean[aggDF$CO2_trt=="eC"],
                         y=leafp.eP.eC.pos, yend=leafp.eP.eC.neg), lwd=1)+
        geom_segment(aes(x=leafn.eP.aC.pos, xend = leafn.eP.aC.neg,
                         y=aggDF$Leaf_P_eP.mean[aggDF$CO2_trt=="aC"], yend=aggDF$Leaf_P_eP.mean[aggDF$CO2_trt=="aC"]), lwd=1)+
        geom_segment(aes(x=leafn.eP.eC.pos, xend = leafn.eP.eC.neg,
                         y=aggDF$Leaf_P_eP.mean[aggDF$CO2_trt=="eC"], yend=aggDF$Leaf_P_eP.mean[aggDF$CO2_trt=="eC"]), lwd=1)+
        labs(y="Leaf P conc. (%)", x="Leaf N conc. (%)")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(0, 0.8))+
        scale_x_continuous(limits=c(0, 8))+
        ggtitle("a")+
        scale_color_manual(name=paste("Vegetation type"),
                           limits=c("Woody", "Nonwoody"),
                           values=c("darkgreen", "orange"),
                           labels=c("Woody", "Non-woody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation type"),
                          limits=c("Woody", "Nonwoody"),
                          values=c("darkgreen", "orange"),
                          labels=c("Woody", "Non-woody"))+     
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p1)
    
    p2 <- ggplot()+ 
        geom_line(data=ablines, aes(x=x, y=y1), lty=2)+
        geom_line(data=ablines, aes(x=x, y=y2), lty=3)+
        annotate(geom="text", x=7.5, y=0.45, label="1:16 line",
                 color="black", size=10)+
        annotate(geom="text", x=7.0, y=0.35, label="1:20 line",
                 color="black", size=10)+
        geom_point(data=plotDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, color = as.factor(Vegetation_type)), 
                   size=6, shape=21, alpha=0.9)+
        geom_point(data=plotDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, fill = as.factor(Vegetation_type)), 
                   size=6, shape=21, color="black", alpha=0.9)+
        geom_line(data=plotDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, group=as.factor(Group)),
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"), alpha=0.9)+
        geom_point(data=aggDF, mapping=aes(x=Leaf_N_aP.mean, y=Leaf_P_aP.mean), 
                   size = 5, shape=22, color="black", fill="grey")+
        geom_line(data=aggDF, mapping=aes(x=Leaf_N_aP.mean, y=Leaf_P_aP.mean), size=2,
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
        geom_segment(aes(x=aggDF$Leaf_N_aP.mean[aggDF$CO2_trt=="aC"], xend = aggDF$Leaf_N_aP.mean[aggDF$CO2_trt=="aC"],
                         y=leafp.aP.aC.pos, yend=leafp.aP.aC.neg), lwd=1)+
        geom_segment(aes(x=aggDF$Leaf_N_aP.mean[aggDF$CO2_trt=="eC"], xend = aggDF$Leaf_N_aP.mean[aggDF$CO2_trt=="eC"],
                         y=leafp.aP.eC.pos, yend=leafp.aP.eC.neg), lwd=1)+
        geom_segment(aes(x=leafn.aP.aC.pos, xend = leafn.aP.aC.neg,
                         y=aggDF$Leaf_P_aP.mean[aggDF$CO2_trt=="aC"], yend=aggDF$Leaf_P_aP.mean[aggDF$CO2_trt=="aC"]), lwd=1)+
        geom_segment(aes(x=leafn.aP.eC.pos, xend = leafn.aP.eC.neg,
                         y=aggDF$Leaf_P_aP.mean[aggDF$CO2_trt=="eC"], yend=aggDF$Leaf_P_aP.mean[aggDF$CO2_trt=="eC"]), lwd=1)+
        labs(y="Leaf P conc. (%)", x="Leaf N conc. (%)")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(0, 0.6))+
        scale_x_continuous(limits=c(0,8))+
        scale_color_manual(name=paste("Vegetation type"),
                           limits=c("Woody", "Nonwoody"),
                           values=c("darkgreen", "orange"),
                           labels=c("Woody", "Non-woody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation type"),
                          limits=c("Woody", "Nonwoody"),
                          values=c("darkgreen", "orange"),
                          labels=c("Woody", "Non-woody"))+        
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2)
    
    pdf("output/step1/Figure_2.pdf", width=10, height=12)
    plot_grid(p1, p2,
              #rel_widths=c(0.5, 1, 1, 0.9),
              rel_heights=c(1,1.4),
              labels=c(""), ncol=1, align="v", axis = "l")    
    dev.off()
    
}
