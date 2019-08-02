plot_leaf_N_P_concentration_comparison <- function() {
    
    ### pass in data
    inDF <- reDF100[reDF100$Variable%in%c("Leaf N concentration", "Leaf P concentration"),]
    
    write.csv(inDF, "output/metafor_summary_plot/leaf_N_P_concentration.csv", row.names=F)
    
    ### read in post-processing dataset
    inDF <- read.csv("output/metafor_summary_plot/leaf_N_P_concentration_input.csv")
    
    
    ### prepare 1: 16 and 1:20 line
    ablines <- data.frame(seq(0.1, 10, by=0.1), NA, NA)
    colnames(ablines) <- c("x", "y1", "y2")
    ablines$y1 <- ablines$x / 16
    ablines$y2 <- ablines$x / 20
    
    
    ### aggregated arrow
    aggDF <- summaryBy(Leaf_N_aP+Leaf_N_eP+Leaf_P_aP+Leaf_P_eP~CO2_trt, data=inDF, FUN=c(mean, sd), keep.names=T)
    
    
    
    ### make plot
    p1 <- ggplot()+ 
        geom_line(data=ablines, aes(x=x, y=y1), lty=2)+
        geom_line(data=ablines, aes(x=x, y=y2), lty=3)+
        annotate(geom="text", x=4, y=0.25, label="1:16 line",
                 color="black", size=10)+
        annotate(geom="text", x=5.2, y=0.27, label="1:20 line",
                 color="black", size=10)+
        geom_point(data=inDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, color = as.factor(Group)), 
                   size=6, shape=21)+
        geom_point(data=inDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, fill = as.factor(Group)), 
                   size=6, shape=21, color="black")+
        geom_line(data=inDF, mapping=aes(x=Leaf_N_eP, y=Leaf_P_eP, group=as.factor(Group)),
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
        geom_point(data=aggDF, mapping=aes(x=Leaf_N_eP.mean, y=Leaf_P_eP.mean), 
                   size = 10, shape=22, color="black", fill="grey")+
        geom_line(data=aggDF, mapping=aes(x=Leaf_N_eP.mean, y=Leaf_P_eP.mean), size=4,
                  arrow = arrow(length=unit(0.50,"cm"), ends="first", type = "closed"))+
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
        scale_y_continuous(limits=c(0, 0.3))+
        scale_x_continuous(limits=c(1,7))+
        ggtitle("HP treatment")+
        guides(fill = guide_legend(title.position = "top"))
    
    plot(p1)
    
    p2 <- ggplot()+ 
        geom_line(data=ablines, aes(x=x, y=y1), lty=2)+
        geom_line(data=ablines, aes(x=x, y=y2), lty=3)+
        annotate(geom="text", x=4, y=0.25, label="1:16 line",
                 color="black", size=10)+
        annotate(geom="text", x=5.3, y=0.27, label="1:20 line",
                 color="black", size=10)+
        geom_point(data=inDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, color = as.factor(Group)), 
                   size=6, shape=21)+
        geom_point(data=inDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, fill = as.factor(Group)), 
                   size=6, shape=21, color="black")+
        geom_line(data=inDF, mapping=aes(x=Leaf_N_aP, y=Leaf_P_aP, group=as.factor(Group)),
                  arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
        geom_point(data=aggDF, mapping=aes(x=Leaf_N_aP.mean, y=Leaf_P_aP.mean), 
                   size = 10, shape=22, color="black", fill="grey")+
        geom_line(data=aggDF, mapping=aes(x=Leaf_N_aP.mean, y=Leaf_P_aP.mean), size=4,
                  arrow = arrow(length=unit(0.50,"cm"), ends="first", type = "closed"))+
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
        scale_y_continuous(limits=c(0, 0.3))+
        scale_x_continuous(limits=c(1,7))+
        #scale_color_manual(name=paste("Study"),
                           #limits=c("woody", "nonwoody"),
                           #values=c("black", "grey"),
                           #labels=c("Woody", "Nonwoody"),
        #                   guide = FALSE)+
        #scale_fill_manual(name=paste("Study"),
                          #limits=c("woody", "nonwoody"),
                          #values=c("black", "grey"),
        #                  labels=c("Woody", "Nonwoody"))+        
        ggtitle("LP treatment")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    pdf("output/metafor_summary_plot/Leaf_N_P_concentration.pdf", width=8, height=12)
    plot_grid(p1, p2,
              #rel_widths=c(0.5, 1, 1, 0.9),
              rel_heights=c(1,1,1.4),
              labels=c(""), ncol=1, align="v", axis = "l")    
    dev.off()
    
}
