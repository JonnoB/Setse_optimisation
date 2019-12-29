#' Create tension matrix
#' 
#' Calculates the edge tension in the network. This is a helper function and is seldom called directly
#' @param Adjmat A matrix. the adjacency matrix of the network
#' @param kmat A numeric matrix. The spring stiffness matrix for all nodes in the network.
#' @param dmat A numeric matrix. The horizontal distance matrix for all nodes in the network.
#' @param zvect A numeric vector. The node elevation.
#' 
#' @export
Create_Tension_matrix3 <- function(ten_mat, zvect, zvect_t, dvect, kvect, non_empty_index){
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

  #this takes the zvect and effectively subtracts the matrix from its transpose. However, a vector of the non-zero cells only is used
  #As the matrix is sparse this greatly reduces the calculations
  dzvect <- zvect_t - zvect #The difference in height between adjacent nodes 
  
  #the hypotenuse of the spring distance triangle
  Hvect <- sqrt(dzvect^2 + dvect^2)
  
  #the tension vector. the dZvect/Hvect is the vertical component of the tension
  ten_mat[non_empty_index] <- kvect*(Hvect-dvect)*dzvect/Hvect
  #arma::mat A = A.replace(datum::nan, 0); #Armadillo code to replace nans caused by dividing by 0. The sparse matrix question remains though
  return (ten_mat)
}
