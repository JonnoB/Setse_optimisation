#' Balance Power network
#'
#'Ensures that the demand and generation of the power network are balanced. This is used before power
#'   flow is calculated.
#'
#' @param g An igraph object representing a power network. A character vector.
#' @param  DemandVar The variable in g that contains the demand of each node. A character vector.
#' @param GenerationVar The variable in g that contains the generation of each node. A character vector.
#' @param OutputVar The desired output variable for the balanced net power demand of each node.
#'    A character vector the default is set to "BalencedPower".
#' @export
#' @examples
#' BalencedGenDem(g, DemandVar, GenerationVar, OutputVar = "BalencedPower")
BalencedGenDem2 <- function(g, DemandVar, GenerationVar, OutputVar = "BalencedPower"){
  #Balences the generation and demand across multiple isolated componants and removes all dead islands
  #the function takes a graph as an argument and outputs a graph
  #g: the graph of the network will have power balanced
  #DemandVar: The Variable name to be used for calculating nodal demand, character vector
  #GenerationVar: The Variable name to be used for calculating nodal Generation, character vector
  #OutputVar: The name of the variable that will be the new balanced power this is a character string
  
  
  component_vect <- components(g)$membership
  Demand <- get.vertex.attribute(g, DemandVar)
  Generation <- get.vertex.attribute(g, GenerationVar)
  
  #a logical in component order
  #A live component has both demand and generation nodes
  # component_is_live <- 1:max(component_vect) %>% map_lgl(~{
  #   
  #   sum(Demand[component_vect==.x])!=0 & sum(Generation[component_vect==.x])!=0
  #   
  # })
  
  #total generation in the component
  comp_gen <- 1:max(component_vect) %>% map_dbl(~{
    
    sum(Generation[component_vect==.x])
    
  }) %>% {.[component_vect]}
  #total demand in the component
  comp_dem <- 1:max(component_vect) %>% map_dbl(~{
    
    sum(Demand[component_vect==.x])
    
  }) %>% {.[component_vect]}
  
  #the vector saying if each node is on a live component
  component_is_live <- (comp_gen!=0) & (comp_dem !=0)
 # stopifnot(component_is_live)
 # print(max(component_vect) )
#print(paste(comp_gen, comp_dem))
  
  #which is greater for the component of this node? generation or demand?
  #Dem_greater_than_Gen <- comp_dem > comp_gen

  
  #If total Demand is bigger than total generation then demand needs to be scaled down
  #the result is multiplied by the logical value so that deadislands are 0.. it's possible this can be removed
  Demand <-  ifelse(comp_dem > comp_gen, Demand*(comp_gen/comp_dem), comp_dem)*(!component_is_live)
  Generation <- ifelse(comp_dem < comp_gen, Generation*(comp_dem/comp_gen), Generation)*(!component_is_live)

  g <- set.vertex.attribute(g, OutputVar, value =Generation - Demand )
  g <- delete.vertices(g, (1:vcount(g))[!component_is_live] )
  return(g)
  
  
}
