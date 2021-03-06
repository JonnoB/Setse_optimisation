#' Create stabilosed blocks
#' 
#' decomposes the network into bi-connected components using articulation points. This speeds up the convergence process
#' and reduces the chances of the SETS algorithm diverging
#' 
#' @param g An igraph object
#' @param Origin block
#' @param OriginBlock_number An integer. this is the origin block chosen from the
#' create_stable_blocks function. Usually this will be the largest block.
#' @param force A character string. This is the node attribute that contains the force the nodes exert on the network.
#' @param flow A character string. This is the edge attribute that is the power flow on the edges.
#' @param capacity A character string. This is the edge attribute that is the flow limit of the edges.
#' @param edge_name A character string. This is the edge attribute that contains the edge_name of the edges.
#' @param tstep A numeric. The time interval used to iterate through the network dynamics.
#' @param tol A numeric. The tolerance factor for early stopping.
#' @param distance A character string. The edge attribute that contains the original/horizontal distance between nodes.
#' @param maxIter An integer. The maximum number of iterations before stopping. Larger networks usually need more iterations.
#' @param mass A numeric. This is the mass constant of the nodes in normalised networks this is set to 1.
#' @param verbose Logical. This value sets whether messages generated during the process are supressed or not.
#' 
#' @seealso \code{\link{Create_stabilised_blocks}} \code{\link{Find_network_balance}}
#' @return A dataframe with the height embeddings of the network
#' 
#' 
#' @export
Create_stabilised_blocks2 <- function(g, 
                                     OriginBlock, 
                                     OriginBlock_number, 
                                     force ="net_generation", 
                                     flow = "power_flow", 
                                     capacity = "edge_limit", 
                                     edge_name = "edge_name",  
                                     tstep=0.1, 
                                     tol = 1e-10, 
                                     distance, 
                                     maxIter, 
                                     mass, 
                                     verbose = TRUE){
  #This function finds the z displacement of all the nodes in the network.
  #g and igraph object of the network
  #OriginBlock a dataframe, output of the Find_network_Balance function of the graph, should be stable or almost stable. mass and K of this block
    #should be used as the basis for the rest of the inputs
  #OriginBlock_number the order number of the origin block in the after creating balanced blocks using Create_balanced_blocks
  #tstep time interval for each iteration
  #tol the tolerance of the system for early termination
  #distance the name of the edge atribute that is used to define the horizontal distance between nodes, this must be meaningful for the units or 1
  #maxIter the maxmimum number of Iterations if no eaerly termination possible
  #kbase the minimum k value must be the same as used in OriginBlock
  #kdiff the range of k values, must be the same as used in OriginBlock
  #mass the mass of the nodes, must be the same as the origin block
  
  #Seperate out the graph into balanced blocks
  #This step will have already been done, but it is fast and simplifies the requirements for the function
  List_of_BiConComps <- create_balanced_blocks(g, force = force, flow = flow)
  
  #remove the Origin block so it doesn't have to be calculated again
  BlockNumbers <-(1:length(List_of_BiConComps))[-OriginBlock_number]
  
  StabilModels <- BlockNumbers %>% 
    map(~{

      Out <- Find_network_balance3(List_of_BiConComps[[.x]],
                                  force =force, 
                                  flow = flow, 
                                  capacity = capacity,  
                                  tstep = tstep, 
                                  tol = tol, 
                                  distance = distance, 
                                  edge_name = edge_name,
                                  maxIter =  maxIter, 
                                  mass =  mass, 
                                  sparse = FALSE,
                                  verbose = FALSE)
      
      #print if the print requirement is on otherwise silent
      if(!verbose){print(paste("Block" ,.x, "of", max(BlockNumbers) ,"termination", nrow(Out$network_dynamics) )) }
      
      return(Out)
      
    })
  
  #get the block tree of the graph
  Block_tree <- biconnected_components(g)
  
  #extract the articulation nodes
  ArticulationVect <- get.vertex.attribute(g, "name", Block_tree$articulation_points)
  
  #place all nodes relative to the origin
  relative_blocks1 <- 1:length(StabilModels) %>% 
    map_df(~{
      print(.x)
      StabilModels[[.x]]$NodeStatus %>%
        mutate(Reference_ID = .x)
      
    }) %>%
    bind_rows(OriginBlock$NodeStatus %>% 
                mutate(Reference_ID = 0)) %>%
    mutate(Articulation_node = (node %in% ArticulationVect ))
  
  #get the network_dynamics dataframe for the total calculation
  network_dynamics <- 1:length(StabilModels) %>% 
    map_df(~{
      print(.x)
      StabilModels[[.x]]$network_dynamics 
      
    }) %>%
    bind_rows(OriginBlock$network_dynamics) %>%
    group_by(t) %>%
    summarise_all(sum)
  
  
  #The height of each node relative to the origin and normalised
  NodeStatus <-fix_z_to_origin(relative_blocks1, ArticulationVect) %>%
    group_by(node) %>%
    summarise_all(mean) %>%
    mutate(Articulation_node = Articulation_node==1)
  
  #comvibe the Nodestatus and the network_dynamics into a single list
  Out <- list(NodeStatus = NodeStatus, network_dynamics = network_dynamics)
  
  return(Out)
  
}