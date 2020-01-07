#'Preapre data for the `find network balance` function
#'
#'This is a helpfer function that makes `find_network_balance` code easier to read. It prepares the data into three datasets.
#'Whether stuff is a character string or not needs to be double checked!
#'
#'The file outputs a named list containing 
#'
#'@param g An igraph object of the network that is going to be turned into a spring embedding.
#'@param force a character string. The node attribute that contains the force information for the network.
#'@param distance a character string. The edge attribute that contains the distance of the edge.
#'@param mass a character string. the node attribute that contains the mass of the nodes.
#'@param edge_name a character string. The edge attribute that contains the names of all the edges.
#'@export

Prepare_data_for_find_network_balance3 <-function(g, force, flow, distance, mass, edge_name = edge_name){
  #this is a helper function that goes inside the the find network balance function to help make the code easier to read
  
  g <- set.edge.attribute(g, "distance", value = get.edge.attribute(g, distance))
  
  #Create the edge node matrix. This is used to create several sub products
  EdgeNode <- as_data_frame(g) %>% 
    select(edge_name, from, to) %>% 
    gather(key = type, Node, -edge_name) %>%
    arrange(Node) %>%
    mutate(value = ifelse(type =="from", 1, -1)) %>%
    ungroup %>%
    select(-type) %>%
    spread(key = Node, value, fill = 0) %>%
    arrange(edge_name)
  
  rowdat <- EdgeNode$edge_name
  
  EdgeNode <- EdgeNode %>% select(-edge_name) %>%
    as.matrix()
  
  rownames(EdgeNode) <-rowdat
  
  rm(rowdat)
  
  #create the adjacency matrix
  abs_edge <- abs(EdgeNode) 
  t_edge <- t(abs_edge)
  Adj <- (t_edge %*% diag(1, nrow = nrow(abs_edge)) %*% abs_edge)
  Adj <-(Adj !=0)*1
  diag(Adj) <-0
  
  
  NodeStatus <- as_data_frame(g, what = "vertices") %>%
    select(node = name, force = force ) %>%
    mutate(
      z = 0,
      mass = mass,
      NetTension = 0, velocity = 0, 
      friction = 0,
      NetForce = force + NetTension - friction,
      acceleration = NetForce/mass,
      Delta_acceleration = 0,
      t = 0) %>%
    arrange(node)
  
  Link <- as_data_frame(g)  %>%
    rename(flow = flow) %>%
    mutate(EdgeName = .data[[edge_name]], #The edge name has to be flexible so .data is used
           k = Area*E/distance) %>% #This sets a floor and ceiling 
    #to the k values. the more highly loaded a line is the more it should stretch. as LL varies between 0, no loading (stiffness)
    #to 1, overload point, (most elastic). The larger kdiff is the larger the difference in elasticity for highly and lightly loaded lines.
    #Very large kdiff means very little elasticty on lightly loaded lines
    select(EdgeName, distance, k) %>%
    arrange(EdgeName)
  
  
  kmat <- t_edge %*% diag(Link$k, nrow = nrow(Link)) %*% abs_edge
  dmat <- t_edge %*% diag(Link$distance, nrow = nrow(Link)) %*% abs_edge #dvect and kvect are the same
  
  Out <- list(NodeStatus, Link, Adj, kmat, dmat)
  names(Out) <- c("NodeStatus", "Link", "Adjmat", "kmat", "dmat")
  
  return(Out)
  
}