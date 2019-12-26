#' Calculate system dynamics
#' 
#' This is a helper function that calculates the dynamics at time t of all the nodes in the system.
#' It is seldom called on it's own but is called by other functions.
#' 
#' The output of the function is a dataframe that is the next iteration of the NodeStatus dataframe the function recieves.
#' 
#' @param NodeStatus A data frame The current dynamics and forces experienced by the node a data frame.
#' @param EdgeNode 
#' @param kvect A numeric vector of the spring stiffnesses
#' @param dvect A numeric vector of the initial distances between the nodes
#' @param tstep A numeric value. The time step, measured in seconds, that will be used to calculate the new dynamic state
#' @param frctmultiplier A numeric value. Used to set a multiplier on the friction value. Generally leave this alone.
#'
#' @export

Calc_System_Dynamics2 <- function(NodeStatus, Adjmat, kmat, dmat, tstep = 1, frctmultiplier = 1){
  
  Tension_vect <- Create_Tension_matrix2(Adjmat, kmat, dmat, NodeStatus$z) %>% rowSums()
  Friction_vect <- Calc_Damping_matrix2(Adjmat, NodeStatus$velocity, kmat, NodeStatus$mass) %>% rowMeans()
  
  # NodeStatus2 <- NodeStatus %>%
  #   mutate(z = Distance(z, velocity, acceleration, t0 = t, t1 = t + tstep),
  #          velocity = velocity(velocity, acceleration, t, t + tstep),
  #          NetTension =  Tension_vect,
  #          friction = Friction_vect*frctmultiplier, #velocity*10,
  #          NetForce = force + NetTension - friction,
  #          acceleration = NetForce/mass,
  #          Delta_acceleration = (acceleration-NodeStatus$acceleration)/tstep, #Find change in acceleration, used in early termination
  #          t = t + tstep)
  
  #This old school method is much faster than using mutate
  NodeStatus2 <- NodeStatus
  NodeStatus2$z <- Distance(NodeStatus$z, NodeStatus$velocity, NodeStatus$acceleration, t0 = NodeStatus$t, t1 = NodeStatus$t + tstep)
  NodeStatus2$velocity <- velocity(NodeStatus$velocity, NodeStatus$acceleration, NodeStatus$t, NodeStatus$t + tstep)
  NodeStatus2$NetTension <- Tension_vect
  NodeStatus2$friction <- Friction_vect*frctmultiplier
  NodeStatus2$NetForce <- NodeStatus2$force + NodeStatus2$NetTension - NodeStatus2$friction
  NodeStatus2$acceleration <- NodeStatus2$NetForce/NodeStatus2$mass
  NodeStatus2$Delta_acceleration <- (NodeStatus2$acceleration-NodeStatus$acceleration)/tstep
  NodeStatus2$t <- NodeStatus2$t+tstep

  
  paste("acceleration", sum(abs(NodeStatus2$acceleration)), "velocity", sum(abs(NodeStatus2$velocity)))
  
  return(NodeStatus2)
  
}