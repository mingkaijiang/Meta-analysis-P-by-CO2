plot_paper_clusters <- function() {
    
    ### require
    require(tidyverse)
    require(network)
    
    ppDF <- read_csv("data/P_by_CO2_paper_meta_data_numbers.csv", na="NA")
    
    Pfrom <- ppDF %>%
        distinct(Pfrom) %>%
        rename(label = Pfrom)
    
    Pto <- ppDF %>%
        distinct(Pto) %>%
        rename(label = Pto)
    
    
    nodes <- full_join(Pfrom, Pto, by = "label")
    
    #nodes <- nodes %>% rowid_to_column("id")
    
    per_route <- ppDF %>%  
        group_by(Pfrom, Pto) %>%
        ungroup()
    
    edges <- per_route %>% 
        left_join(nodes, by = c("Pfrom" = "label")) %>% 
        rename(from = "Pfrom")
    
    edges <- edges %>% 
        left_join(nodes, by = c("Pto" = "label")) %>% 
        rename(to = "Pto")
    
    edges <- select(edges, from, to, weight)
    
    #routes_network <- network(edges, vertex.attr = nodes, 
    #                          matrix.type = "edgelist", ignore.eval = FALSE)
    
    #plot(routes_network, vertex.cex = 1, displaylabels = TRUE)
    
    require(igraph)
    
    net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    
    #net <- add_vertices(net, 14, name=c("Almeida et al. 1999",
    #                                   "Campbell & Sage 2002",
    #                                   "Imai & Adachi 1996",
    #                                   "Jakobsen et al. 2016",
    #                                   "Newbery et al. 1995",
    #                                   "Pandey et al. 2015",
    #                                   "Prior et al. 2003",
    #                                   "Sicher 2005",
    #                                   "Syvertsen & Graham 1999",
    #                                   "Thompson et al. 2019",
    #                                   "Walker et al. 1995",
    #                                   "Whitehead et al. 1997",
    #                                   "Wolf 1996",
    #                                   "Gentile et al. 2012"))
    
    net <- add_vertices(net, 14, name=c("1",
                                        "3",
                                        "19",
                                        "21",
                                        "27",
                                        "29",
                                        "30",
                                        "33",
                                        "38",
                                        "40",
                                        "42",
                                        "43",
                                        "44",
                                        "15"))
    
    
    clp <- cluster_label_prop(net)

    V(net)$community <- clp$membership
    rain <- rainbow(24, alpha=.5)
    V(net)$color <- rain[V(net)$community]
    
    #pdf("output/step1/Figure_1c.pdf")
    #plot(clp, net, vertex.size=8, 
    #     edge.arrow.size=.4,layout=layout_with_fr,
    #     vertex.label.color="black", 
    #     vertex.label.cex=0.8)
    #dev.off()
    
    pdf("output/step1/Figure_1d.pdf")
    plot(net, vertex.size=10, 
         edge.arrow.size=.4,vertex.label.cex=1,
         layout=layout_in_circle,  
         edge.curved=.3,vertex.color="grey")
    dev.off()
    
}
