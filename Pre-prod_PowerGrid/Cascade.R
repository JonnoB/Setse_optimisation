#' Calculate the effects of a cascade
#'
#' Iterates through the network removing edges until no edges are over the line limit and the network has stabilised.
#'
#' This is a recursive function that calls itself repeatedly until the cascade has come to a stop and the network has stabilised.
#'   The function produces a list of graphs showing each state of the cascade. This function is mostly used as part of the
#'   AttackTheGrid function.
#' @param NetworkList A list of power-grid networks.
#' @param Iteration The current iteration number
#' @param StopCascade The number of iterations when cascade will be forced to terminate. An integer, default is set to infinity.
#' @param Demand the name of the node Load variable. A character string.
#' @param Generation The name of the node generation variable. A character string.
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow A character string. This value indicates the name of the edge attribute that holds power flow, the default is "PowerFlow"
#' @param edge_limit A character string. This value indicates the name of the edge attribute that holds the edge limit, the default is "Link.Limit"
#' @export
#' @seealso \code{\link{AttackTheGrid}}
#' Cascade(NetworkList,
#' Iteration = 0,
#' StopCascade = Inf,
#' g0 = NULL,
#' Generation = "Generation",
#' Demand = "Demand",
#' EdgeName = "Link",
#' VertexName = "name",
#' Net_generation = "BalencedPower",
#' power_flow = "PowerFlow",
#' edge_limit = "Link.Limit"
#' )


Cascade <- function(NetworkList,
                                Iteration = 0,
                                StopCascade = Inf,
                                g0 = NULL,
                                AZero,
                                LineProperties,
                                Generation = "Generation",
                                Demand = "Demand",
                                EdgeName = "Link",
                                VertexName = "name",
                                Net_generation = "BalencedPower",
                                power_flow = "PowerFlow",
                                edge_limit = "Link.Limit"
){

  #Iteration: the number of iteration number of the cascade, used to keep track of what is going on
  CascadeContinues <- TRUE
  while(CascadeContinues & Iteration != StopCascade){
    
    g <- NetworkList[[length(NetworkList)]]
    
    Iteration <- Iteration + 1
    
    comp_info <- components(g)
    
    #if graph comparison is being used then the process starts here.
    #This stops needless subgraphs being recalculated then joined.
    #This cannot be the first iteration and there needs to be more than one component
    if(!is.null(g0) & comp_info$no>1){
      #find components that need to be recalculuated
      RecalcFlow <- Components_differ(g, g0, EdgeName = EdgeName)
      #save the full graph
      g_temp <- g
      #create a subgraph of elements that do not need to be recalculated
      g <- delete.vertices(g,( 1:vcount(g))[!(comp_info$membership %in% RecalcFlow)])

    }
    
    #calculate the power flow on the subgraph
    g <- PowerFlow(g, AZero, LineProperties, 
                                EdgeName = EdgeName, 
                                VertexName = VertexName, 
                                Net_generation = Net_generation, 
                                power_flow = power_flow)
    
    if(!is.null(g0)& comp_info$no>1){
      #add back in power flow to the full graph
      changed_edge_index <- match(edge_attr(g, EdgeName), edge_attr(g_temp, EdgeName) )
      
      g <- set_edge_attr(g_temp, name = power_flow, index =  changed_edge_index, value = edge_attr(g, power_flow) )

    }
    
    
    #Delete Edges that are over the Limit
    
    edge_index_over <- (1:ecount(g))[abs(edge_attr(g, name = power_flow)) > edge_attr(g, name = edge_limit)]
    #if no edges are over the limit setting the DeleteEdges to NA prevents errors being thrown
    DeleteEdges <- edge_attr(g, name = EdgeName, index = edge_index_over)

    #If the cascade has been going for more than 1 round then the overloaded edges need to be added together
    if(Iteration==1){
      Overloads <- DeleteEdges

    } else {
      Overloads <- c(graph_attr(g, "EdgesOverloaded" ), DeleteEdges)

    }
    
    #write vector of overloaded edges
    g <- set_graph_attr(g, "EdgesOverloaded", Overloads)

    g2<- delete.edges(g, edge_index_over)

    #Balance grid after over powered lines and edges are removed
    g2 <- BalancedGenDem(g2, Demand, Generation, OutputVar = Net_generation)
    
    #Checks to see if there are any changes in the edges of the network.
    #As no new edges can appear if the number of edges in the two graphs is identical then 
    #the graph structures must be identical. Sole vertices are irrelevant for the purposes of a cascade
    #In additiona any lone vertices lacking power or demand would have already been removed.
    #If all the edges are the same returns TRUE
    edgesequal <-ecount(g2) == ecount(g)
    
    #Terminates the cascade if there are no edges left preventing errors.
    GridCollapsed<- ecount(g2)==0
    #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
    CascadeContinues <- !isTRUE(edgesequal) & !GridCollapsed
    
    g0 <- g
    
    if(CascadeContinues & Iteration != StopCascade){
      #add the new network into the list
      NetworkList <- c(NetworkList, list(g2))
      
    }
  }
  #message(paste("Cascade has completed with", Iteration, "iterations"))
  
  return(NetworkList)
  
}
