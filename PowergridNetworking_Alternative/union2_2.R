#' Merge two graphs
#'
#' Merges two graphs maintaining whilst thier attributes.
#'
#' This function is an upgrade to the \pkg{igraph} function "union".
#'    It creates the union of two networks, and merges attributes with the same name.
#'    In the original union the graph attributes were lost.
#'    In the case where there are overlapping nodes the attributes of g1 take precedence
#'
#' @return A new graph object
#' @param g1 Igraph object
#' @param g2 Igraph object
#' @seealso \code{\link[igraph]{union.igraph}}
#' @export
#' @examples
#' union2(g1,g2)
#' Reduce(union2, list(g1,g2,g3))
#'
union22<-function(g1, g2){

  #Internal function that cleans the names of a given attribute
  #alll this parsing is a bad idea. should just call the functions directly and have a bigger loop.
  #There is a non parse version of this, but it doesn't seem to get the speeds ups i'f hoped for. 
  #Also I can'r be bothered to test it atm

  #The explicit igraph function needed to be included as it was being overwritten by other functions causing an error
  g <- igraph::union(g1, g2)
  #loop through each attribute type in the graph and clean
 # g <- CleanNames(g) #cleannames two version not currently used

    g <- CleanNames_2(g)

  return(g)

}
