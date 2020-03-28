plot_author_clusters <- function() {
    
    ### require
    require(tidyverse)
    require(network)
    
    inDF <- read_csv("data/P_by_CO2_meta_data.csv")
    
    Authors <- inDF %>%
        distinct(Authors) %>%
        rename(label = Authors)
    
    Literature <- inDF %>%
        distinct(Literature) %>%
        rename(label = Literature)
    
    
    nodes <- full_join(Authors, Literature, by = "label")
    
    #nodes <- nodes %>% rowid_to_column("id")
    
    per_route <- inDF %>%  
        group_by(Literature, Authors) %>%
        summarise(weight = n()) %>% 
        ungroup()
    
    edges <- per_route %>% 
        left_join(nodes, by = c("Authors" = "label")) %>% 
        rename(from = "Authors")
    
    edges <- edges %>% 
        left_join(nodes, by = c("Literature" = "label")) %>% 
        rename(to = "Literature")
    
    edges <- select(edges, from, to, weight)
    
    routes_network <- network(edges, vertex.attr = nodes, 
                              matrix.type = "edgelist", ignore.eval = FALSE)
    
    #plot(routes_network, vertex.cex = 1, #displaylabels = TRUE,
    #     vertex.col = rep(c(2, 5), times=c(117,166)))
    
    require(igraph)
    
    net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    

    clp <- cluster_label_prop(net)
    class(clp)
    
    plot(clp,net, vertex.size=4, edge.arrow.size=.2,
         edge.color="orange", 
         vertex.color="gray50", 
         vertex.label=NA)#vertex.color="orange")#, 
         #vertex.frame.color="#ffffff",
         #vertex.label=V(net)$Literature, vertex.label.color="black")
    
    #V(net)$community <- clp$membership
    #colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
    #plot(net, vertex.color=colrs[V(net)$community])
    
    
    
}
