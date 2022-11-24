rm(list = ls())
library(png)
library(igraph)

nodes <- read.csv("C:/Users/paul/Documents/polnet2016/Data files/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("C:/Users/paul/Documents/polnet2016/Data files/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]

nodes2 <- read.csv("C:/Users/paul/Documents/polnet2016/Data files/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("C:/Users/paul/Documents/polnet2016/Data files/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net

plot(net, edge.arrow.size=.4,vertex.label=NA)

net2 <- graph_from_incidence_matrix(links2)

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6
# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA
# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  25
  l <- do.call(layout, list(net))
  plot(net, edge.arrow.mode=0, layout=l, main=layout)
}
