#' Initiate power-grid attack simulation
#'
#' This function simulates an attack on the power grid using the parameter settings you choose
#'    the outut of the function is a nested list of igraph objects.
#' @param NetworkList A list of lists where each element of the sub list is an igraph object, the first time it is used the
#' the network list is simply list(list(g)).
#' @param AttackStrategy A function that calculates which node to delete the function is is in "quo" form and embedded in an
#' attack type.
#' @param referenceGrid The grid that will be used to test the largest component against if NULL it uses the given network.
#' @param MinMaxComp The minimum size of the maximum component, as a fraction, for the process to continue, the default is set
#' to 0.0 complete collapse.
#' @param TotalAttackRounds The maximum number of nodes to be removed before the process stops.
#' @param CascadeMode Whether the power flow equations will be used to check line-overloading or not.
#' @param Demand the name of the node Load variable. A character string.
#' @param Generation the name of the node generation variable. A character string.
#' @param EdgeName the variable that holds the edge names, a character string.
#' @param VertexName the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation the name that the net generation data for each node is held in
#' @param power_flow A character string. This value indicates the name of the edge attribute that holds power flow, the default is "PowerFlow"
#' @param edge_limit A character string. This value indicates the name of the edge attribute that holds the edge limit, the default is "Link.Limit"
#' @export
#' @examples
#' AttackTheGrid(NetworkList, AttackStrategy, SubstationData, EdgeData, referenceGrid = NULL, MinMaxComp = 0.8)
#' Out <- AttackTheGrid(NetworkList,
#' AttackStrategy,
#' referenceGrid = NULL,
#' MinMaxComp = 0.8,
#' TotalAttackRounds=100,
#' CascadeMode = TRUE,
#' CumulativeAttacks = NULL)

#This version builds on the Azero version but also replaces compdiff
#with a non loop-version
AttackTheGrid_compdiff_noloop <- function(g,
                          AttackStrategy,
                          referenceGrid = NULL,
 #                         MinMaxComp = 0.0,
                          TotalAttackRounds=1000,
                          CascadeMode = TRUE,
                          Demand = "Demand",
                          Generation = "Generation",
                          EdgeName = "Link",
                          VertexName = "name",
                          Net_generation = "BalencedPower",
                          power_flow = "PowerFlow",
                          edge_limit = "Link.Limit"
                          ){

  
  #I can change the function so that only a graph need be entered and a list of graphs is returned. This is
  #becuase I am no longer recursing the function.
  
  NetworkList <- vector(mode = "list", length = TotalAttackRounds)
  NetworkList[[1]] <- list(g)
  #Entering a graph instead of a list would make this tosh simpler
  #g <- NetworkList[[length(NetworkList)]]
  
  #g <- g[[length(g)]]
  #This if statement only needs to be done once not every time. A small change but makes it easier to read.
  if(is.null(referenceGrid)){
    referenceGrid  <- g
  }
  
  #precalculation of the line and transmission matrices
  #This speeds up the PTDF function by reducing expensive operations (Transmission more than LineProperties)
  AZero <- CreateTransmission(g, EdgeName, VertexName)
  LineProperties <- LinePropertiesMatrix(g, EdgeName, Weight = "Y")
  
  FractGC <- 1
  GridCollapsed <- FALSE
  TopoStability <- FALSE
  CumulativeAttacks <- 0
  
  #The stop conditdions are a little over the top
  while (!(CumulativeAttacks==TotalAttackRounds| GridCollapsed| TopoStability)) {
    
    CumulativeAttacks <- CumulativeAttacks + 1
    #print(CumulativeAttacks)
    #gets the last network in the list
    #gc()
    g <- NetworkList[[CumulativeAttacks]]
    
    g <- g[[length(g)]]

    #Remove the desired part of the network.
    gCasc <- AttackStrategy %>% 
      eval_tidy(., data = list(g = g)) #The capture environment contains delete nodes, however the current g is fed in here
    
    
    ##Rebalence network
    # #This means that the Cascade calc takes a balanced network which is good, generation or demand nodes may have been removed
    #this needs to be accounted for
    gCasc <- BalencedGenDem(gCasc, Demand, Generation, OutputVar = Net_generation)
    
    GridCollapsed <- ecount(gCasc)==0
    
    gCasc <- list(gCasc)

    
    #This If statement prevents Cascading if theire are no cascadable components
    if(!GridCollapsed){
      
      if(CascadeMode){
        #this returns a list of networks each of the cascade
        gCasc <- Cascade_compdiff_noloop(NetworkList  = gCasc,
                         Iteration = 0,
                         StopCascade = Inf,
                         g0 = g,
                         AZero = AZero,
                         LineProperties = LineProperties,
                         Demand = Demand,
                         Generation = Generation,
                         EdgeName = EdgeName,
                         VertexName = VertexName,
                         Net_generation = Net_generation,
                         power_flow = power_flow,
                         edge_limit = edge_limit
        )
        
      }
      
      message(paste("Attack ",CumulativeAttacks, " Nodes Remaining", vcount(gCasc[[length(gCasc)]])))
      
    } else{
      
      message("Grid collapsed simulation complete")
      
    }
    
    
    #concatanate the new list with the list of lists
    NetworkList[[CumulativeAttacks+1]] <-gCasc
    
    #extract the last network from the just completed cascade
    gCascLast <- gCasc[[length(gCasc)]]
    
    #If the largest componant is larger than the MinMaxComp threshold
    #call the function again and delete a new node.
    
    #when the grid has collapsed problems arise this helps deal with that
    #MaxComp <- suppressWarnings(max(components(gCascLast)$csize))
    
    #Checks to see if the topology of the network is unchanged.
    #If this is TRUE then nothing is being removed and the process can stop
    TopoStability <- (vcount(gCascLast) == vcount(g) &   ecount(gCascLast) == ecount(g))
    
    #FractGC <-ifelse(is.finite(MaxComp),MaxComp/vcount(referenceGrid), 0)
    
  }
  
  

  return(NetworkList)
}