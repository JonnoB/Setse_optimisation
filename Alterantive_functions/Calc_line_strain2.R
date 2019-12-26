#' Calculate line strain
#' 
#' This function calculates the line strain characteristics for a graph
#' 
#' @param g An igraph object of the network
#' @param solved_heigh_df A data frame. This is the results of Create_stabilised_blocks or Find_network_balance
#' @param distance A character string. The name of the edge attribute that contains the distance between two nodes. The default is "distance"
#' @param capcity A character string. The name of the edge attribute that contains the flow capacity of the edge between two nodes. 
#' @param flow A character string. The name of the edge attribute that contains the flow between the two nodes at the end of the edge. The default is "power_flow".
#' @param edge_name   A character string. The name of the edge attribute that contains the edge name. The default is "edge_name".
#' @export

Calc_line_strain2 <- function(edge_df, solved_height_df, merge_order_df, distance = "distance", capacity, flow = "power_flow", edge_name = "edge_name"){

  #convert the character strings to symbols
  #afterwords the symbols can be evaluated by curly curly {{}}
  #Really replacing the sym function with inline .data[[]] would be better but I'll have to leave that for another time
  # edge_orig <- edge_name #keeps it in original string form
  # flow_orig <- flow
  # distance <- sym(distance)
  # capacity <- sym(capacity)
  # flow <- sym(flow)
  # edge_name <- sym(edge_name)
  #node_height_df <-solved_height_df[,c("node", "z")]# %>% select(node, z) #It is much faster not to use select
  edge_df$from_z <- solved_height_df$z[merge_order_df$from_z]
  edge_df$to_z <- solved_height_df$z[merge_order_df$to_z]

  #base is faster than dplyr, when it comes to lots of iterations it adds up
  Out <- edge_df
  Out$dz <- abs(Out$from_z-Out$to_z)
  Out$mean_z <- ( Out$from_z+ Out$to_z)/2
  Out$H <- sqrt(Out$dz^2 + Out[,distance]^2)
  Out$strain <- (Out$H-Out[,distance])/Out[,distance]
  Out$alpha <- Out[,capacity]/abs(Out[,flow])
  Out$line_load <- abs(Out[,flow])/Out[,capacity]
  Out$percentile_strain = percent_rank(Out$strain)
  
  
  # Out <- edge_df %>%
  #   mutate(dz = abs(from_z-to_z),
  #          mean_z = (from_z+to_z)/2,
  #          H = sqrt(dz^2 +({{distance}})^2),
  #          strain = (H-{{distance}})/{{distance}},
  #          alpha = {{capacity}}/abs({{flow}}),
  #          line_load = abs({{flow}})/{{capacity}},
  #          percentile_strain = percent_rank(strain))

  Out <- Out[,c(edge_name, "alpha", "line_load", "dz", "H", "strain", "percentile_strain", "mean_z", flow)]
   # select({{edge_name}}, alpha, line_load, dz, H, strain, percentile_strain, mean_z, {{flow}}) ] #It is much faster not to use select
  
  return(Out)
  
}