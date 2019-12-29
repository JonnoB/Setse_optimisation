#' Calculate line strain
#' 
#' This function calculates the line strain characteristics for a graph. It ia a helper function of 
#' find_stabil_system and is rarely called by the ser. It is a much faster version of'calc_tension_strain'.
#' 
#' @param g An igraph object of the network
#' @param solved_heigh_df A data frame. This is the results of Create_stabilised_blocks or Find_network_balance
#' @param distance A character string. The name of the edge attribute that contains the distance between two nodes. The default is "distance"
#' @param capcity A character string. The name of the edge attribute that contains the flow capacity of the edge between two nodes. 
#' @param flow A character string. The name of the edge attribute that contains the flow between the two nodes at the end of the edge. The default is "power_flow".
#' @param edge_name   A character string. The name of the edge attribute that contains the edge name. The default is "edge_name".
#' @export

Calc_line_strain2 <- function(edge_df, solved_height_df, merge_order_df, distance = "distance"){
  
  ##
  #This is the matrix version
  ##
  edge_df[,2] <- solved_height_df[merge_order_df[,1],2]
  edge_df[,3] <- solved_height_df[merge_order_df[,2],2]
  
  
  #base is faster than dplyr, when it comes to lots of iterations it adds up
  edge_df[,4] <- abs(edge_df[,2]-edge_df[,3])
  edge_df[,5] <- sqrt(edge_df[,4]^2 + edge_df[,1]^2)
  edge_df[,6]<- (edge_df[,5]-edge_df[,1])/edge_df[,1]
  
  # #node_height_df <-solved_height_df[,c("node", "z")]# %>% select(node, z) #It is much faster not to use select
  # edge_df$from_z <- solved_height_df$z[merge_order_df$from_z]
  # edge_df$to_z <- solved_height_df$z[merge_order_df$to_z]
  # 
  # 
  # #base is faster than dplyr, when it comes to lots of iterations it adds up
  # edge_df$dz <- abs(solved_height_df$from_z-edge_df$to_z)
  # edge_df$H <- sqrt(edge_df$dz^2 + edge_df[,distance]^2)
  # edge_df$strain <- (edge_df$H-edge_df[,distance])/edge_df[,distance]

  return(edge_df)
  
}