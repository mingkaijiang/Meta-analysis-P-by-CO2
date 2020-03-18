make_step2_interaction_funnel_plot <- function(res, var.name) {
    
    pdf(paste0("output/step2/supplementary/funnel_", var.name, ".pdf"),
        height=8, width=8)
    funnel(res)
    dev.off()
}