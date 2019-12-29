#' Calculate damping matrix
#' This function calculates the damping experienced by each node based on it's contact edges and current speed.
#' This is a helper function and is seldom called on it's own
#' 
#' @param EdgeNode The edgenode matrix
#' @param v_vect the velocity of each node
#' @param kvect the vector of spring stiffness for each edge
#' @param dvect, the vector of horizontal distance between the springs.
#' @export
#' 
Calc_Damping_matrix3 <- function(damp_mat, v_vect, kmat, mvect){
  
  #Create the damping matrix
  ##Create the velocity vector
  Vvect <- NodeStatus[non_empty_matrix[,1],5]
  
  #calculate the critical damping of each edge and put back into matrix format
  damp_mat[non_empty_matrix[,3]]<- 2*sqrt(kvect*NodeStatus[non_empty_matrix[,1],3])*NodeStatus[non_empty_matrix[,1],5]
  #Ften_mat[!is.finite(Ften_mat)] <-0 #There cannot be any infinite values
  
  return(Ften_mat)
}