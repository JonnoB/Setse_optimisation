#' Find the network balance
#' 
#' This simulates the dynamics of the network for a set number of iterations or until convergence which ever is the sooner.
#' 
#' This function is often used in conjunction with \code{Create_stabilised_blocks} and \code{create_balanced_blocks}
#' 
#' @param g An igraph object
#' @param force A character string. This is the node attribute that contains the force the nodes exert on the network.
#' @param flow A character string. This is the edge attribute that is the power flow on the edges.
#' @param capacity A character string. This is the edge attribute that is the flow limit of the edges.
#' @param distance A character string. The edge attribute that contains the original/horizontal distance between nodes.
#' @param edge_name A character string. This is the edge attribute that contains the edge_name of the edges.
#' @param tstep A numeric. The time interval used to iterate through the network dynamics.
#' @param mass A numeric. This is the mass constant of the nodes in normalised networks this is set to 1.
#' @param max_iter An integer. The maximum number of iterations before stopping. Larger networks usually need more iterations.
#' @param coef_drag A numeric. This sets the multiplier of friction. Only use if you want to be annoyed and confused
#' @param tol A numeric. The tolerance factor for early stopping.
#' @param sparse Logical. Whether or not the function should be run using sparse matrices. must match the actual matrix, this could prob be automated
#' @param verbose Logical. This value sets whether messages generated during the process are supressed or not.
#' @param two_node_solution Logical. The 
#' 
#' @return A list of two elements. A data frame with the height embeddings of the network as well as the convergence dynamics dataframe for the network
#' @seealso \code{\link{Create_stabilised_blocks}} \code{\link{create_balanced_blocks}}
#' @export

Find_network_balance5 <- function(g, 
                                 force ="net_generation", 
                                 flow = "power_flow", 
                                 capacity = "capacity", 
                                 distance = "distance", 
                                 edge_name = "edge_name",
                                 tstep = 0.02, 
                                 mass = 1, 
                                 max_iter = 20000, 
                                 coef_drag = 1, 
                                 tol = 1e-6,
                                 sparse = FALSE,
                                 two_node_solution = TRUE){

  #helper function that prepares the data
  Prep <- Prepare_data_for_find_network_balance3(g = g, 
                                                 force = force, 
                                                 flow = flow, 
                                                 distance = distance, 
                                                 mass = mass, 
                                                 edge_name = edge_name,
                                                 sparse = sparse)
  
  #do special case solution I should change this to a standalone function for ease of reading but it isn't important
  if(nrow(Prep$Link)==1 & two_node_solution){
    
    if(Prep$node_status$force[1]==0 &Prep$node_status$force[2]==0){
      
      solution_angle <-0
      
    } else {
      #uses the non-linear optimiser from minpack.lm to find the solution to the two node special case, this is much faster
      solution_angle <- nlsLM(Force ~ ForceV_from_angle(target_angle, k = k, d = d), 
                              start = c(target_angle = pi/4), 
                              data = list(Force = abs(Prep$node_status$force[1]), k = Prep$Link$k, d = Prep$Link$distance), 
                              upper = pi/2) %>% coefficients()      
      
    }
    
    temp <- Prep$node_status %>%
      mutate(elevation = ifelse(force>0, 
                        tan(solution_angle)/2, #height above mid point
                        -tan(solution_angle)/2 ), #height below mid-point
             net_force = 0,
             acceleration = 0
      ) 
    
    Out <- list(network_dynamics = tibble(t = 0, 
                                 Iter = 0,
                                 static_force = 0, 
                                 kinetic_force = 0), 
                node_status = temp
    )
    #Solves using the iterative method.
  } else{
    
    Out <- FindStabilSystem_test(
      node_status = Prep$node_status, 
      ten_mat = Prep$ten_mat, 
      non_empty_matrix = Prep$non_empty_matrix, 
      kvect = Prep$kvect, 
      dvect = Prep$dvect, 
      mass = mass,
      tstep = tstep, 
      max_iter = max_iter, 
      coef_drag = coef_drag,
      tol = tol, 
      sparse = sparse) 
    
  }
  
  
  return(Out)
  
}
