#' Create tension matrix
#' 
#' Calculates the edge tension in the network. This is a helper function and is seldom called directly
#' @param Adjmat A matrix. the adjacency matrix of the network
#' @param kmat A numeric matrix. The spring stiffness matrix for all nodes in the network.
#' @param dmat A numeric matrix. The horizontal distance matrix for all nodes in the network.
#' @param zvect A numeric vector. The node elevation.
#' 
#' @export
Create_Tension_matrix2 <- function(Adjmat, kmat, dmat, zvect){
  #Creates a matrix showing the forces exerted by the contracting spring for each edge of a node
  #positive numbers pull down (like the force of mg) and negative forces pull up
  #The v_vect, kvect and mvect all have to be ordered alphanumerically!
  #EdgeNode <- The edgenode matrix 
  #zvect the height of each node
  #kvect the vector of spring stiffness for each edge
  #dvect, the vector of horizontal distance between the springs.
  
  #example
  # SmallEdgeNode <- matrix(c(1,-1,0,
  #                           0,1,-1), nrow = 2, byrow = T)
  # Smallzvect <- c(2,-1,1)
  # Smallkvect <- c(3,4)
  # Smalldvect <- c(1,1)
  # 
  # Create_Tension_matrix(SmallEdgeNode, Smallzvect, Smallkvect, Smalldvect)

  
  Zmat <- Adjmat*zvect #The adjacency matrix weight by node height
  dZmat <- t(Zmat)- Zmat #The difference in height between adjacent nodes
  
  Hmat <- sqrt(dZmat^2 + dmat^2)
  
  Ften_mat <- kmat*(Hmat-dmat)*dZmat/Hmat
  Ften_mat[!is.finite(Ften_mat)] <-0
  
  return (Ften_mat)
}
