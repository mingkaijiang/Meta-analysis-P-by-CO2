make_step5_interaction_leave_one_out_plot <- function(res, res2, looDF, var.name) {
    
    plotDF <- as.data.frame(looDF)
    
    #dat <- with(density(plotDF$estimate), data.frame(x, y))
    #
    #min.value <- as.numeric(res2$b-res2$se)
    #max.value <- as.numeric(res2$b+res2$se)
    
    
    #p1 <- ggplot(data = dat, mapping = aes(x = x, y = y)) +
    #    geom_line()+
    #    geom_area(mapping = aes(x = ifelse(x>min.value & x< max.value , x, 0)), fill = "red", alpha=0.5)+
    #    geom_vline(aes(xintercept=res2$b), color="black", 
    #               linetype="dashed", size=1)
    
    
    ### make plotting script
    p1 <- ggplot(plotDF, aes(x=estimate)) +
        geom_density() +
        geom_vline(aes(xintercept=res2$b), color="black", 
                   linetype="dashed", size=1)
    
    #plot(p1)
    
    ### plot
    pdf(paste0("output/step5/supplementary/leave1out_", var.name, ".pdf"),
        height=8, width=8)
    
    plot(p1)
    
    dev.off()
}