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

Cascade_fix <- function(NetworkList,
                    Iteration = 0,
                    StopCascade = Inf,
                    g0 = NULL,
                    Generation = "Generation",
                    Demand = "Demand",
                    EdgeName = "Link",
                    VertexName = "name",
                    Net_generation = "BalencedPower",
                    power_flow = "PowerFlow",
                    edge_limit = "Link.Limit"
){
  #This Function iterates through the network removing edges until there are no further overpower edges to remove
  #This function uses the Bus order to choose the slack reference should this be changed?
  #Iteration: the number of iteration number of the cascade, used to keep track of what is going on
  
  g <- NetworkList[[length(NetworkList)]]
  
  Iteration <- Iteration + 1
  #message(paste("Using previous graph", !is.null(g0)))
  
  #if graph comparison is being used then the process starts here.
  #This stops needless subgraphs being recalculated then joined.
  if(!is.null(g0)){
    #find components that need to be recalculuated
    RecalcFlow <-  (1:components(g)$no)[Components_differ_fix(g, g0, EdgeName = EdgeName)]
    #create a subgraph of elements that do not need to be recalculated
    gNochange <- delete.vertices(g, (1:vcount(g))[components(g)$membership %in% RecalcFlow])
    #create a subgraph of parts that do need to be recalculated
    g <- delete.vertices(g,( 1:vcount(g))[!(components(g)$membership %in% RecalcFlow)])
    #  message(paste("components changed since previous", paste(RecalcFlow, collapse = ",")))
  }
  
  
  #Calculate new power flow over all edges after target has been removed
  #... This function does not calculate whether the lines are over the limit 
  g <- CalcOverLimit(g,  EdgeName, VertexName, Net_generation, power_flow = power_flow)
  
  #If there is a reference graph that has subcomponents the subcomponents that have been changed by targeting are re-combined into the
  #rest of the network here
  if(!is.null(g0)){
    g <- union2(gNochange, g)
  }
  
  #Delete Edges that are over the Limit
  
  
  DeleteEdges <- as_data_frame(g) %>%
    mutate(index = 1:n(),
           Over.Limit = abs(.data[[power_flow]]) > .data[[edge_limit]]) %>%
    filter(Over.Limit)
  
  #If the cascade has been going for more than 1 round then the overloaded edges need to be added together
  if(Iteration==1){
    Overloads <- DeleteEdges %>% pull(EdgeName)
    
  } else {
    
    Overloads <- c(graph_attr(g, "EdgesOverloaded" ), DeleteEdges %>% pull(EdgeName))
    
  }
  
  #write vector of overloaded edges
  g <- set_graph_attr(g, "EdgesOverloaded", Overloads)
  
  #g is structurally changed here and becomes g2
  g2 <- delete.edges(g, DeleteEdges$index)
  
  #Balence grid after over powered lines and edges are removed
  g2 <- BalencedGenDem(g2, Demand, Generation, OutputVar = Net_generation)
  
  #Checks to see if there are any changes in the edges of the network by component.
  #If all the edges are the same returns TRUE
  edgesequal <-all(!Components_differ_fix(g2, g, EdgeName = EdgeName))
  
  #Terminates the cascade if there are no edges left preventing errors.
  GridCollapsed<- ecount(g2)==0
  #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
  CascadeContinues <- !isTRUE(edgesequal) & !GridCollapsed
  
 # message(paste("Iteration", Iteration, "Edges equal", edgesequal ))
  
  if(CascadeContinues & Iteration != StopCascade){
    #add the new network into the list
    NetworkList <- c(NetworkList, list(g2))
    #update the list with the new lists created in the cascade
    NetworkList <- Cascade_fix(NetworkList,
                           Iteration = Iteration,
                           StopCascade = StopCascade,
                           g0 = g,
                           Generation = Generation,
                           Demand = Demand,
                           EdgeName = EdgeName,
                           VertexName = VertexName ,
                           Net_generation = Net_generation,
                           power_flow = power_flow,
                           edge_limit = edge_limit
    )
  }
  
  message(paste("Cascade has completed with", Iteration, "iterations"))
  
  return(NetworkList)
  
}
