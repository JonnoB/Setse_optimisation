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
Calc_Damping_matrix2 <- function(Adjmat, v_vect, kmat, mvect){
  
  #The v_vect, kvect and mvect all have to be ordered alphanumerically!
  #EdgeNode <- The edgenode matrix 
  #v_vect the velocity of each node
  #kvect the vector of spring stiffness for each edge
  #dvect, the vector of horizontal distance between the springs.
  
  #Creat the adjacency matrix

  Vmat <- Adjmat*v_vect #The adjacency matrix weight by node height

  Ften_mat<- 2*sqrt(kmat*mvect)*Vmat
  Ften_mat[!is.finite(Ften_mat)] <-0
  
  return((Ften_mat))
}