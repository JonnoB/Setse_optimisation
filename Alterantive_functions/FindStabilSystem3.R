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


FindStabilSystem3 <- function(g, distance, NodeStatus, Adjmat, flow, kmat, dmat, capacity, edge_name = edge_name, 
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
  NodeList <- as.matrix(NodeStatus[,-1])
  
  #gets the dimensions of the matrix for bare bones column sum
  m <- dim(NodeList)
  
 #Sparse matrix mode reduces time and memory requirements for larger matrices 100 nodes used dense 300 use sparse
  if(sparse){
  ten_mat <- damp_mat <- Matrix(Adjmat, sparse = T)} else{
    ten_mat <- damp_mat <- Adjmat
  }
  
  #results <- as.list(rep(NA,maxIter))
  network_dynamics <- matrix(data = NA, nrow = maxIter, ncol = 5) %>%
    as_tibble() %>%
    set_names(c("Iter","t", "force_energy", "kinetic_energy", "strain")) %>%
    as.matrix()
  # results <- matrix(data = NA, nrow = maxIter, ncol = 10) %>%
  #   as_tibble() %>%
  #   set_names(c("t", "z", "NetForce", "velocity", "acceleration", "max_accel", 
  #               "max_Delta_accel", "friction", "strain", "kinetic_en")) %>%
  #   as.matrix()
  
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
    
    #sparse or dense mode chosen by user on basis of network size and of course sparsity
    if(sparse){
    temp <- Calc_System_Dynamics_sparse3(NodeList, 
                                  ten_mat = ten_mat, 
                                  damp_mat = damp_mat, 
                                  kvect, dvect,  
                                  mat_size = m[1], 
                                  tstep, 
                                  non_empty_matrix = non_empty_matrix, 
                                  frctmultiplier)
    }else{
      
      temp <- Calc_System_Dynamics3(NodeList, 
                                           ten_mat = ten_mat, 
                                           damp_mat = damp_mat, 
                                           kvect, dvect,  
                                           mat_size = m[1], 
                                           tstep, 
                                           non_empty_matrix = non_empty_matrix, 
                                           frctmultiplier)
      
    }
    

    
    #calculates the line strain each round
    #flow, edge_name, capacity are not used if alpha, capacity and percentile strain are not used.
    #This is a special compact version of line strain that is extra fast It has all extraneous data removed
    line_strain <- Calc_line_strain2(edge_mat, 
                                    solved_height_df = temp,
                                    merge_order_df = merge_order_df
                                    )
    
    network_dynamics[Iter,]<-  c(Iter,
                        Iter*tstep, #time in seconds
                        sum(abs(temp[,7])),  #force energy, the total amount of energy that will be used per second in the given state same as net force
                        sum(0.5*temp[,3]*temp[,5]^2 ),#kinetic_energy. mass is constant for all nodes so could be a scaler
                        sum(line_strain[,6])
                        ) #sum of line strain in the system      
    #pre simplification of the results matrix
    # results[Iter,]<-  c(Iter*tstep, 
    #   .colMeans(abs(temp[,c(2,7,5,8)]), m = m[1],n =  4), 
    #   max(abs(temp[,8])),
    #   max(abs(temp[,9])), 
    #   max(abs(temp[,6])), 
    #   sum(line_strain[,6])/number_edges,#mean(line_strain[,6]) #faster mean... although this is kind of not worth it
    #   sum(0.5*temp[,3]*temp[,5]^2))
#Removed resukts temporarily. Result doesn't work as the temp object is now a matrix not a df
    
  #   results[Iter,] <- c(temp$t[1],
  #                       mean(abs(temp$z)),
  #                       mean(abs(temp$NetForce)),
  #                       mean(abs(temp$velocity)),
  #                       mean(abs(temp$acceleration)),
  #                       max(abs(temp$acceleration)),
  #                       max(abs(temp$Delta_acceleration)),
  #                       max(abs(temp$friction)),
  #                       mean(line_strain$strain)
  #                       )
  #    # results[Iter,1:8]<- temp %>%
  #    #  summarise(t = mean(t),
  #    #            z = mean(abs(z)),
  #    #            NetForce = mean(abs(NetForce)),
  #    #            velocity = mean(abs(velocity)),
  #    #            acceleration = mean(abs(acceleration)),
  #    #            max_accel = max(abs(acceleration)),
  #    #            max_Delta_accel = max(abs(Delta_acceleration)),
  #    #            friction = max(abs(friction)))
  #    # results[Iter,9] <- mean(line_strain$strain)
  # 
    NodeList <- temp
    
    #check if system is stableusing the acceleration and max acceleration
    if(is.infinite(network_dynamics[Iter,3])| is.infinite(network_dynamics[Iter,4])| is.infinite(network_dynamics[Iter,5])){ #if there are infinte values terminate early
      system_stable <- TRUE
    } else{
      system_stable <- (sum(network_dynamics[Iter,3:4]) < tol)
    }

    if(verbose){

      print(paste("Iteration", Iter,
                "strain", signif(network_dynamics[Iter,5], 3),
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
