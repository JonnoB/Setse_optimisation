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
#' @param friction_stop Logical. Includes friction as a stopping condition. useful in some situations
#' @export


FindStabilSystem2 <- function(g, distance, NodeStatus, Adjmat, flow, kmat, dmat, capacity, edge_name = edge_name, 
                             tstep, maxIter = 1000, frctmultiplier = 1, 
                              tol = 1e-10, verbose = TRUE, friction_Stop= FALSE ){
  #Runs the physics model to find the convergence of the system.
  
  #friction_stop fricton is a stopping condition. defualts to FALSE. 
  NodeList <- NodeStatus
  
  #results <- as.list(rep(NA,maxIter))
  results <- matrix(data = NA, nrow = maxIter, ncol = 9) %>%
    as_tibble() %>%
    set_names(c("t", "z", "NetForce", "velocity", "acceleration", "max_accel", 
                "max_Delta_accel", "friction", "strain"))
  
  #prep the graph matrix so it doesn't need to be done in the loop
  edge_df <-  as_data_frame(g) %>%
    mutate(from_z = NA,
           to_z = NA,
           dz = NA,
           mean_z = NA,
           H = NA,
           strain = NA,
           alpha = NA,
           line_load = NA,
           percentile_strain = NA)
  
  #find the data frame order of the 'from' and 'to nodes to merge the solved_height_df to the 
  #First get the final order of the Calc_dynamics_function. This method isn't great but it is a small cost as the function is called many
  #times
  temp <- NodeList %>%
    Calc_System_Dynamics2(., Adjmat, kmat, dmat, tstep, frctmultiplier = 1)
  
  merge_order_df <-tibble( from_z =match(edge_df$from, temp$node),
                           to_z =match(edge_df$to, temp$node))
  rm(temp)
  Iter <- 1
  system_stable <- FALSE
  
  while((Iter <= maxIter) & !system_stable ){
    
    # print(NodeList[[n]])
    temp <- Calc_System_Dynamics2(NodeList, Adjmat, kmat, dmat, tstep, frctmultiplier)
    

    
    #calculates the line strain each round
    #flow, edge_name, capacity are not used if alpha, capacity and percentile strain are not used.
    line_strain <- Calc_line_strain2(edge_df, 
                                    solved_height_df = temp,
                                    merge_order_df = merge_order_df,
                                    distance = distance, 
                                    capacity = capacity, 
                                    flow = flow,
                                    edge_name = edge_name
                                    )

    results[Iter,] <- c(temp$t[1],
                        mean(abs(temp$z)),
                        mean(abs(temp$NetForce)),
                        mean(abs(temp$velocity)),
                        mean(abs(temp$acceleration)),
                        max(abs(temp$acceleration)),
                        max(abs(temp$Delta_acceleration)),
                        max(abs(temp$friction)),
                        mean(line_strain$strain)
                        )
     # results[Iter,1:8]<- temp %>%
     #  summarise(t = mean(t),
     #            z = mean(abs(z)),
     #            NetForce = mean(abs(NetForce)),
     #            velocity = mean(abs(velocity)),
     #            acceleration = mean(abs(acceleration)),
     #            max_accel = max(abs(acceleration)),
     #            max_Delta_accel = max(abs(Delta_acceleration)),
     #            friction = max(abs(friction)))
     # results[Iter,9] <- mean(line_strain$strain)

    NodeList <- temp

    system_stable <- (results$max_accel[Iter] < tol & results$max_Delta_accel[Iter] < tol)#check if system is stable
    
    if(friction_Stop){

      system_stable <- system_stable & results$friction[Iter] < tol #includes friction as a stopping condition. useful in some situations
    }
    
    
    if(verbose){

      print(paste("Iteration", Iter, 
                "strain", signif(results$strain[Iter], 3),
                  "z", signif(results$z[Iter], 3),
                  " velocity", signif(results$velocity[Iter], 3), 
                  "max acceleration", signif(results$max_accel[Iter], 3), 
                  "friction",  signif(results$friction[Iter], 3) )
      ) # print result
      
    }
    
    Iter <- Iter + 1 # add next iter

  }
  
  #empty elements are a logical NA value. otherwise they are a triple classed tibble. so can be filtered out.. tortourous and probably risky

  Out <- results %>%
    filter(!is.na(z)) %>%
    list(., NodeList)

  names(Out) <- c("results", "NodeList")
  
  return(Out)

}
