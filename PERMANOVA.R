

library(PERMANOVA
)



Localidades$loc<-as.factor(Localidades$PROVINCIA)

X=IniTransform(hongos)
D = DistContinuous (X)
perwine=PERMANOVA(D,Localidades$loc,
                  CoordPrinc = T, dimens = 1, PCoA = "Standard",
                  DatosIni = TRUE, PostHoc="none" )
perwine
plot.PERMANOVA(perwine)

summary(perwine)

sum(perwine$ExplainedVariance)
perwine$DistMuestral
library(permute
)
library(vegan)
# Perform permutation-based dissimilarity
library(permute)
perm_diss <- vegdist(bact,upper = T,diag = T)

# Compute network layout
library(igraph)
net_layout <- layout.circle(graph_from_adjacency_matrix(perm_diss, mode = "directed"))
plot(net_layout)
matrixbact
# Plot permutation network
library(permNet)

plot(permNet(perm_diss, layout = net_layout), edge.width = 2, edge.color = "gray")


# Example dissimilarity matrix
diss_matrix <- matrix(c(0, 0.2, 0.4, 0.2, 0, 0.6, 0.4, 0.6, 0), nrow = 3, byrow = TRUE)

# Example grouping variable
groups <- factor(c("A", "B", "A"))

# Compute network layout
library(igraph)
net_layout <- layout.circle(graph_from_adjacency_matrix(perm_diss, mode = "undirected"))

# Create network object
library(ggnetwork)
net <- ggnetwork(perm_diss)
ggn
# Set grouping variable as a vertexa attribute
network.vertex.names(net) -> rownames(diss_matrix)
network.vertex.names(net$mel ) ->net$mel

# Plot permutation network
library(ggplot2)
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "gray") +
  geom_nodes(aes(fill = ), shape = 21, size = 5) +
  scale_fill_manual(values = c("A" = "blue", "B" = "red")) +
  theme_void()







if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("lefser")

ids::adjective_animal(n=5,style = "snake")


c<-data.frame()
fix(c)
c$Amphibians_spp<-data.matrix(18)

library(ggplot2)
ggplot(DB,aes( x=N,y=row.names(DB)))+
  geom_bar()+theme_classic()

barplot(t(DB),cex.names = T)
fix(DH)
DH<-DH[-5,]
barplot(t(DH),cex.names = T )

