library(purrr)

compute_properties <- function(g){

  #Should we check heck if the graph is undirected?
  #Which one of these algorithms works with undirected/directed?
  eb <- edge.betweenness.community(g)
  fg <- fastgreedy.community(g)
  lp <- label.propagation.community(g)
  le <- leading.eigenvector.community(g)
  ml <- multilevel.community(g)
  cfg <- cluster_fast_greedy(g)
  sg <- spinglass.community(g)
  wt <- walktrap.community(g)
  im <- infomap.community(g)

  clusterings <- list(eb,fg,lp,le,ml,cfg,sg,wt,im)
  clusterings_names <- c('edge betweenness', 'fast greedy', 'label propagation',
                 'leading eigenvector', 'multi level', 'cluster fast greedy', 'springlass',
                 'walktrap', 'infomap')

  par(mfrow=c(2,5))
  # par(mar=c(0.5, 4.5, 1, 1))
  par(mar=c(0,0,5,0))
  layout <- layout_nicely(g)

  plot(g, layout=layout)
  title("Original")

  for(i in 1:9) {
    plot(clusterings[[i]], g, layout=layout)
    title(clusterings_names[i])
  }

  column_names <- c('#Clusters', 'TPR', 'expansion', 'conductance', 'modularity')

  criteria_matrix <- data.frame(row.names = clusterings_names)


  criteria_matrix[1:9,1] <- unlist(map(clusterings, length))
  criteria_matrix[1:9,2] <- unlist(map(clusterings, (function (x) compute_tpr(g, x))))
  criteria_matrix[1:9,3] <- unlist(map(clusterings, (function (x) compute_expansion(g, x))))
  criteria_matrix[1:9,4] <- unlist(map(clusterings, (function (x) compute_conductance(g, x))))
  criteria_matrix[1:9,5] <- unlist(map(clusterings, modularity))

  colnames(criteria_matrix) <- column_names
  return(criteria_matrix)
}

#Return weighted TPR of a clustering algorithm
compute_tpr <- function(g,clustering){
  graph_size <- vcount(g)
  tpr <- 0
   #Iterate over the found clusters
    for(i in 1:length(clustering)){
      #Return the i-th community in the graph
      community <- induced_subgraph(g, vids = unlist(clustering[i]))
      community_size <- vcount(community)
      community_weight <- community_size/graph_size
      #How many vertexes belong to a triangle
      vertexes_in_triangle <- sum(count_triangles(community)>0)
      #Proportion of nodes belonging to a triangle
      tpr <- tpr + (vertexes_in_triangle/community_size)*community_weight
    }
  return(tpr)
}

compute_expansion <- function(g, clustering) {
  return(compute_expansion_conductance(g, clustering)[[1]])
}

compute_conductance <- function(g, clustering) {
  return(compute_expansion_conductance(g, clustering)[[2]])
}

compute_expansion_conductance <- function(g,clustering){

  #Initialization of useful scrutures
  conductance <- 0
  expansion <- 0
  graph_size <- vcount(g)
  edge_list <- get.edgelist(g, names = TRUE)
  memberships <- membership(clustering)
  nc <- sizes(clustering) %>% as.numeric()
  fc <- rep(0, length(nc))
  mc <- rep(0, length(nc))

  #Iterate over the edges of the network and update
  #FC and MC based on the membership of head and tail of edges
  for(i in 1:nrow(edge_list)){
    cluster1<- memberships[edge_list[i, 1]]
    cluster2 <- memberships[edge_list[i, 2]]

    if(cluster1 == cluster2){
      #If head and tail are in the cluster
      mc[cluster1] <- mc[cluster1] + 1
    }
    else{
      #If head and tail belong to different clusters
      #Increase by 1 the FC of both clusters
      fc[cluster1] <- fc[cluster1] + 1
      fc[cluster2] <- fc[cluster2] + 1
    }
  }
  #Once computed the metrics for all the clusters
  #Compute the value of expansion and conductance for each cluster
  #by weighting for the number of nodes in the cluster
  for(i in 1:length(clustering)){
    f <- fc[i]
    m <- mc[i]
    n <- nc[i]
    weight <- n/graph_size
    conductance <- conductance + weight*(f/(2*m+f))
    expansion <- expansion + weight*(f/n)
    results <- list(expansion,conductance)
  }
  return(results)
}

