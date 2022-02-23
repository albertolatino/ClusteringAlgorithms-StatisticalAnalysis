source("functions.r")
library(igraph)

karate <- graph.famous("Zachary")
dolphins <- read.graph('data/unweighted_social_dolphin_Lusseau.graphml', format = 'graphml')
lesmis_table <- read.csv('data/lesmis.mtx', header = FALSE, sep = ' ')
lesmis_unweighted <- lesmis_table[, c(1, 2)]
lesmis <- graph_from_edgelist(data.matrix(lesmis_unweighted), directed = FALSE)
davis_table <- read.csv('data/Davis_southern_club_women-binary.txt', header = FALSE, sep = ' ')
davis_unweighted <- davis_table[, c(1, 2)]
davis <- graph_from_edgelist(data.matrix(davis_unweighted), directed = FALSE)
davis <- simplify(davis)

graph_names <- c("karate", "dolphins", "Les Miserables", "davis")
graphs <- list(karate, dolphins, lesmis, davis)

comparison_graphs_table <- data.frame()
for (number in 1:4) {
  for (i in 1:2) {
    comparison_graphs_table[graph_names[number], i] <- gorder(graphs[[number]])
    comparison_graphs_table[graph_names[number], i] <- gsize(graphs[[number]])
  }
}
colnames(comparison_graphs_table) <- c('N', 'E')

set.seed(0)
summary(karate)
layout_karate <- layout_nicely(karate)
plot(karate, layout=layout_karate)
criteria_matrix <- compute_properties(karate)

summary(dolphins)
layout_dolphins <- layout_nicely(dolphins)
plot(dolphins, layout=layout_dolphins)
compute_properties(dolphins)

summary(lesmis)
layout_lesmis <- layout_nicely(lesmis)
plot(lesmis, layout=layout_lesmis)
compute_properties(lesmis)

summary(davis)
plot(davis)
layout_davis <- layout_nicely(davis)
plot(davis, layout=layout_davis)
compute_properties(davis)


#TASK2
wikipedia_graph <- read.graph("data/wikipedia.gml", format="gml")
#plot(wikipedia_graph)

# compute the clusters with walktrap algorithm
wikipedia_clustering <- walktrap.community(wikipedia_graph)

#Computing some statistical properties of the network
number_of_clusters <- length(wikipedia_clustering)
print(number_of_clusters)
number_nodes <- vcount(wikipedia_graph)
number_edges <- gsize(wikipedia_graph)
nc <- sizes(wikipedia_clustering) %>% as.numeric()
max_size <- max(nc)
min_size <- min(nc)
clusters_avg_size <- mean(nc)
cluster_std <- sd(nc)

printed <- 0


for(i in 1:number_of_clusters){
  #Find the first 10 clusters with size less than 20
  if(nc[i] < 20 && printed < 10){
    printed <- printed + 1
    selected_cluster <- induced_subgraph(wikipedia_graph, vids = (wikipedia_cluster[i] %>% unlist))
    plot(selected_cluster)
  }
}
