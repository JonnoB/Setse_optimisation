#' Find components of a graph that have changed
#' Find components of a graph that have changed
#'
#' Using this should be able to cut down on the amount of calculation done on the power flow equations
#' This function checks to see which subcomponents have changed in the graph and which are the same.
#' It does this by comparing edges. This is because a component may have had an edge removed but still be intact.
#' However, the powerflow will still need to be re-calculated.
#' The function outputs a logical value
#' @param g the current graph
#' @param g0 the previous graph
#' @export
#'
Components_differ2 <-function(g, g0, EdgeName = "name"){
  #This function checks to see which subcomponents have changed in the graph and which are the same.
  # It does this by comparing edges. This is because a component may have had an edge removed but still be intact.
  # However, the powerflow will still need to be re-calculated.
  #using this should be able to cut down on the amount of calculation done on the power flow equations
  #g the current graph
  #g0 the previous graph
  
  g_list <- components(g)
  g0_list <- components(g0)
  gcomps <- g_list$no #current graph
  g0comps <- g0_list$no #previous graph

  
  #Create a vector that is the component that each edge belongs to for graphs g and g0
  g_df <- as_data_frame(g)
  g_df_edge_membership <-   g_list$membership[match(g_df$from, names(g_list$membership))]
  g0_df <-as_data_frame(g0)
  g0_df_edge_membership <-  g0_list$membership[match(g0_df$from, names(g0_list$membership))]
  
  #the question I am asking here is, 'is B in A?'. this is necessary incase A is a subset of B, because B can never be a subset of A
  #all( g0_df[g0_df_edge_membership==3,EdgeName] %in% g_df[g_df_edge_membership==3,EdgeName])
  
  
  
  if(gcomps==0){
    #This is necessary as in rare occasions a grid collapses meaning there are no Components
    #in such cases the function throws an error. This if statement prevents that
    #Instead returning a 0 meaning the graphs are different which they are.
    dat <- 0
    
  } else{

    #Make a matrix that allows each list to be checked against the other
    dat <- expand.grid(1:gcomps, 1:g0comps)
    
    #The check for a single node prevents empty vectors providing false positives
    dat <- map2_lgl(.x = dat$Var1, .y  = dat$Var2, ~ {
      base::all( g0_df[g0_df_edge_membership==.y,EdgeName] %in% g_df[g_df_edge_membership==.x,EdgeName] #all nodes in component are equal
      & length(g_df[g_df_edge_membership==.x,EdgeName])>0) #and the component has an edge, aka is more than a single node
    }
    ) %>%
  matrix(data = . , nrow = gcomps) %>%
  rowSums
  }
  
  dat == 0
  
}
