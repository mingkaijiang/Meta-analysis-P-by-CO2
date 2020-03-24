make_step5_interaction_funnel_plot <- function(res, var.name) {
    
    pdf(paste0("output/step5/supplementary/funnel_", var.name, ".pdf"),
        height=8, width=8)
    funnel(res)
    dev.off()
}