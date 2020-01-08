update_dynamics <- function(NodeList,
                            network_dynamics,
                            ten_mat, 
                            non_empty_matrix, 
                            kvect, 
                            dvect, 
                            mass,
                            tstep, 
                            coef_drag = 1, 
                            sparse = FALSE,
                            one_vect,
                            Iter){
  
  #calculate the system dynamics. Either sparse or dense mode
  #sparse or dense mode chosen by user on basis of network size and of course sparsity
  
  #The code is not put in sub-functions as this creates memory management problems and half the time
  #the program runs can be spent auto calling gc(). This reduces the copying of data...I think
  #It overwirtes the preious values but doesn't create anything else
  #NodeList2 <- NodeList
  
  #####
  #create the tension matrix
  #####
  #dz is the change in eleveation
  dzvect <- NodeList[non_empty_matrix[,2],2] - NodeList[non_empty_matrix[,1],2] #The difference in height between adjacent nodes 
  
  #the hypotenuse of the spring distance triangle
  Hvect <- sqrt(dzvect^2 + dvect^2)
  
  #the tension vector. the dZvect/Hvect is the vertical component of the tension
  ten_mat[non_empty_matrix[,3]] <- kvect*(Hvect-dvect)*dzvect/Hvect
  
  #The remaining dynamics are calculated here
  
  NodeList[,2] <- NodeList[,4]*tstep +0.5*NodeList[,8]*tstep*tstep + NodeList[,2] #Distance/elevation s1 = ut+0.5at^2+s0
  NodeList[,4] <- NodeList[,4] + NodeList[,8]*tstep #velocity v1 = v0 +at
  NodeList[,6] <- NodeList[,1] + NodeList[,3] #static force 
  
  if(sparse){
    #This uses the matrix row aggregation functions which can be used on sparse matrices. This is faster and much more memory
    #efficient for large matrices
    NodeList[,3] <- Matrix::rowSums(ten_mat) #tension
  }else{
    #This uses the standard dense matrices, this is faster for smaller matrices.
    NodeList[,3] <- ten_mat %*% one_vect  #.rowSums(ten_mat, m = m[1], n = m[1]) #tension
  }
  NodeList[,5] <- coef_drag*NodeList[,4] #friction of an object in a viscous fluid under laminar flow
  NodeList[,7] <- NodeList[,6] - NodeList[,5] #net force
  NodeList[,8] <- NodeList[,7]/mass #acceleration
  NodeList[,9] <- NodeList[,9] + tstep #current time #This may not be neccessary but doesn't really hurt
  
  
  network_dynamics[Iter,]<-  c(Iter, #Iteration
                               Iter*tstep, #time in seconds
                               sum(abs(NodeList[,6])),  #static force. The force exerted on the node
                               sum(abs(0.5*mass*NodeList[,4]/tstep)), #kinetic_force 
                               sum( 0.5*kvect*(Hvect-dvect)^2),     #spring potential_energy
                               sum(0.5*mass*NodeList[,4]^2)    #kinetic_energy
  ) 
  
  
  
  return(list(NodeList = NodeList, network_dynamics = network_dynamics))
}