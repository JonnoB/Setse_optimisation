#' Find stabil system
#' 
#' This function uses the SETS embedding to find the equilibrium position of a network or a bi-connected component
#' @param g An igraph object. The network
#' @param NodeStatus A data frame The current dynamics and forces experienced by the node a data frame.
#' @param EdgeNode 
#' @param flow A character string. This is the edge attribute that is the power flow on the edges.
#' @param kvect A numeric vector of the spring stiffnesses
#' @param dvect A numeric vector of the initial distances between the nodes
#' @param capacity A character string. This is the edge attribute that is the flow limit of the edges
#' @param edge_name A character string. This is the edge attribute that contains the edge_name of the edges.
#' @param tstep A numeric value. The time step, measured in seconds, that will be used to calculate the new dynamic state
#' @param maxIter An integer. The maximum number of iterations before stopping. Larger networks usually need more iterations.
#' @param frctmultiplier A numeric value. Used to set a multiplier on the friction value. Generally leave this alone..s.
#' @param tol A numeric. The tolerance factor for early stopping.
#' @param verbose Logical. This value sets whether messages generated during the process are supressed or not.
#' @export
#' 
#' 
#' In this version the spring damping is removed and friction is for an object in a viscous fluid.
#' This means that Fr = -bv where be is some constant in this case it is frctmultiplier. Appropriate values of b need to be determined 

#caoacity, edge_name and flow are no longer used. If the preprocessing is all done in prep then distance can also be removed

FindStabilSystem4 <- function(g, distance, NodeStatus, Adjmat, flow, kmat, dmat, capacity, edge_name = edge_name, 
                              tstep, maxIter = 1000, frctmultiplier = 1, 
                              tol = 1e-10, sparse = FALSE, verbose = TRUE){
  #Runs the physics model to find the convergence of the system.
  
  #This creates a matrix with the row column position absolute index and transpose index of the edges in the matrix
  #This means vectors can be used for most operation greatly reducing the amount of memory required and 
  #providing a modest speed increase.
  non_empty_matrix <- which(Adjmat!=0, arr.ind = T) %>%
    tibble(names = rownames(.), rows = .[,1], 
           cols = .[,2],
           index = rows+ (cols-1)*ncol(Adjmat),
           t_index = cols + (rows-1)*ncol(Adjmat)) %>% {.[,3:6]} %>%
    as.matrix()
  
  kvect <- kmat[non_empty_matrix[,3]]
  dvect <- dmat[non_empty_matrix[,3]]
  
  
  #friction_stop fricton is a stopping condition. defualts to FALSE. 
  NodeList <- NodeList2 <-as.matrix(NodeStatus[,-1])
  
  #gets the dimensions of the matrix for bare bones column sum
  m <- dim(NodeList)
  
  #Sparse matrix mode reduces time and memory requirements for larger matrices 100 nodes used dense 300 use sparse
  if(sparse){
    ten_mat  <- Matrix(Adjmat, sparse = T)} else{
      ten_mat <- Adjmat
    }
  
  #results <- as.list(rep(NA,maxIter))
  network_dynamics <- matrix(data = NA, nrow = maxIter, ncol = 4) %>%
    as_tibble() %>%
    set_names(c("Iter","t", "static_force", "kinetic_force")) %>%
    as.matrix()

  #prep the graph matrix so it doesn't need to be done in the loop
  edge_mat <-  as_data_frame(g)[,c("from", "to", distance)] %>%
    mutate(
      from_z = NA,
      to_z = NA,
      dz = NA,
      H = NA,
      strain = NA)
  
  #find the data frame order of the 'from' and 'to nodes to merge the solved_height_df to the 
  merge_order_df <-tibble( from_z =match(edge_mat$from, NodeStatus[,1]),
                           to_z =match(edge_mat$to, NodeStatus[,1])) %>%
    as.matrix()
  
  edge_mat <- as.matrix(edge_mat[,-c(1,2)])
  
  number_edges <- nrow(edge_mat)
  
  Iter <- 1
  system_stable <- FALSE
  
  while((Iter <= maxIter) & !system_stable ){
    
    # print(NodeList[[n]])
    #calculate the system dynamics. Either sparse or dense mode
    #sparse or dense mode chosen by user on basis of network size and of course sparsity
    
    #The code is not put in sub-functions as this creates memory management problems and half the time
    #the program runs can be spent auto calling gc(). This reduces the copying of data...I think
    #It overwirtes the preious values but doesn't create anything else
    NodeList2 <- NodeList
    #####
    #create the tension matrix
    #####
    dzvect <- NodeList[non_empty_matrix[,2],2] - NodeList[non_empty_matrix[,1],2] #The difference in height between adjacent nodes 
    
    #the hypotenuse of the spring distance triangle
    Hvect <- sqrt(dzvect^2 + dvect^2)
    
    #the tension vector. the dZvect/Hvect is the vertical component of the tension
    ten_mat[non_empty_matrix[,3]] <- kvect*(Hvect-dvect)*dzvect/Hvect
    
    ####
    ## Create the Damping matrix
    ###
    #damp_mat[non_empty_matrix[,3]]<- 2*sqrt(kvect*NodeList[non_empty_matrix[,1],3])*NodeList[non_empty_matrix[,1],5]
    
    if(sparse){
      #This uses the matrix row aggregation functions which can be used on sparse matrices. This is faster and much more memory
      #efficient for large matrices
      NodeList2[,4] <- Matrix::rowSums(ten_mat) #tension
      #NodeList2[,6] <- Matrix::rowMeans(damp_mat)*frctmultiplier #friction
    }else{
      #This uses the standard dense matrices, this is faster for smaller matrices.
      
      NodeList2[,4] <- .rowSums(ten_mat, m = m[1], n = m[1]) #tension
      # NodeList2[,6] <- .rowMeans(damp_mat, m = m[1], n = m[1])*frctmultiplier #friction
    }
    #The remaining dynamics are calculated here
    
    #If these equations of motion work well then the distance and velocity equations can be removed
    NodeList2[,2] <- NodeList[,5]*tstep +0.5*NodeList[,8]*tstep +NodeList[,2] #Distance  ###This is wrong needs squaring! 
    NodeList2[,5] <- NodeList[,5] + NodeList[,8]*tstep #velocity
    NodeList2[,6] <- frctmultiplier*NodeList2[,5] #friction of an object in a viscous fluid under laminar flow
    NodeList2[,7] <- NodeList2[,1] + NodeList2[,4] - NodeList2[,6] #Netforce
    NodeList2[,8] <- NodeList2[,7]/NodeList2[,3] #acceleration
    NodeList2[,9] <- (NodeList2[,8]-NodeList[,8])/tstep #delta acceleration...this can be removed
    NodeList2[,10] <- NodeList[,10] + tstep 

    
    network_dynamics[Iter,]<-  c(Iter,
                                 Iter*tstep, #time in seconds
                                 sum(abs(NodeList2[,1] + NodeList2[,4])),  #static force. The force exerted on the node
                                 sum(0.5*NodeList2[,3]*NodeList2[,5]/tstep )#kinetic_energy. mass is constant for all nodes so could be a scaler
    ) #sum of line strain in the system      
    
    NodeList <- NodeList2
    
    #check if system is stableusing the acceleration and max acceleration
    if(!is.finite(network_dynamics[Iter,3])| !is.finite(network_dynamics[Iter,4])){ #if there are infinte values terminate early
      system_stable <- TRUE
    } else{
      system_stable <- (network_dynamics[Iter,3] < tol)
    }
    
    if(verbose){
      
      print(paste("Iteration", Iter,
                  "net force", signif(network_dynamics[Iter,3], 3),
                  "kinetic force", signif(network_dynamics[Iter,3], 3))
      ) # print result
      
    }
    
    Iter <- Iter + 1 # add next iter
    
  }
  
  #Early termination causes NA values. These are removed by the below code
  #
  network_dynamics <- as_tibble(network_dynamics) %>%
    filter(complete.cases(.))
  
  Out <- list(as_tibble(network_dynamics), bind_cols(NodeStatus["node"] , as_tibble(NodeList)))
  names(Out) <- c("network_dynamics", "NodeStatus")
  
  return(Out)
  
}