#' Find stabil system
#' 
#' This function uses the SETS embedding to find the equilibrium position of a network or a bi-connected component
#' @param node_status A data frame The current dynamics and forces experienced by the node a data frame.
#' @param ten_mat A data frame The current dynamics and forces experienced by the node a data frame.
#' @param non_empty_matrix A numeric matrix. contains the index of the non-empty cells in the adjacency matrix. see details.
#' @param kvect A numeric vector of the spring stiffnesses
#' @param dvect A numeric vector of the initial distances between the nodes
#' @param mass A numeric. This is the mass constant of the nodes in normalised networks this is set to 1.
#' @param tstep A numeric value. The time step, measured in seconds, that will be used to calculate the new dynamic state
#' @param max_iter An integer. The maximum number of iterations before stopping. Larger networks usually need more iterations.
#' @param coef_drag A numeric value. Used to set a multiplier on the friction value. Generally leave this alone..s.
#' @param tol A numeric. The tolerance factor for early stopping.
#' @param sparse Logical. Whether or not the function should be run using sparse matrices. must match the actual matrix, this could prob be automated
#' @param verbose Logical. This value sets whether messages generated during the process are supressed or not.
#' @export
#' 
#' @details The non_empty matrixhe row column position absolute index and transpose index of the edges in the matrix
#' This means vectors can be used for most operation greatly reducing the amount of memory required and 
#' providing a modest speed increase. The non_empty_matrix is propduced by the 'Prepare_data_for_find_network_balance' function.
#'
# Strips out all pre processing to make it as efficient and simple as possible

#caoacity, edge_name and flow are no longer used. If the preprocessing is all done in prep then distance can also be removed

FindStabilSystem_test <- function(node_status, ten_mat, non_empty_matrix, kvect, dvect, mass,
                             tstep, max_iter = 1000, coef_drag = 1, 
                             tol = 1e-10, sparse = FALSE, verbose = FALSE){
  #Runs the physics model to find the convergence of the system.
  
  
  #friction_stop fricton is a stopping condition. defualts to FALSE. 
  NodeList  <-as.matrix(node_status[,-1])
  
  #gets the dimensions of the matrix for bare bones column sum
  m <- dim(NodeList)
  
  #This dataframe is one of the final outputs of the function, it is premade for memory allocation
  network_dynamics <- matrix(data = NA, nrow = max_iter, ncol = 6) %>%
    as_tibble() %>%
    set_names(c("Iter","t", "static_force", "kinetic_force", "potential_energy", "kinetic_energy")) %>%
    as.matrix()
  
  one_vect <- rep(1, nrow(NodeList))
  
  state <- list(NodeList = NodeList, network_dynamics = network_dynamics)
  
  Iter <- 1
  system_stable <- FALSE
  
  while((Iter <= max_iter) & !system_stable ){
###
###    
### This method is absolutely terrible
###
###    
    state <- update_dynamics(state$NodeList,
                             state$network_dynamics,
                             ten_mat, 
                             non_empty_matrix, 
                             kvect, 
                             dvect, 
                             mass,
                             tstep, 
                             coef_drag, 
                             sparse,
                             one_vect,
                             Iter)
    
    #NodeList <- NodeList2
    
    #check if system is stableusing the acceleration and max acceleration
    
    if(!is.finite(state$network_dynamics[Iter,3])| !is.finite(state$network_dynamics[Iter,4])){ #if there are infinte values terminate early
      system_stable <- TRUE

    } else{
      system_stable <- (state$network_dynamics[Iter,3] < tol)

    }

    Iter <- Iter + 1 # add next iter
    
  }

  #Early termination causes NA values. These are removed by the below code
  #
  network_dynamics <- as_tibble(state$network_dynamics) %>%
    filter(complete.cases(.))
  
  Out <- list(as_tibble(network_dynamics), bind_cols(node_status[,"node",drop=FALSE] , as_tibble(state$NodeList)))
  names(Out) <- c("network_dynamics", "node_status")
  
  return(Out)
  
}