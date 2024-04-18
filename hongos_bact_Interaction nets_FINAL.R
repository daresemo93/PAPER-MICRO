####Bacterias######

bacterias<-bactpara.redes[,-2]
citation("igraph")
library(igraph) # for Network graph
library(visNetwork) # for interaction Network graph
fix(intento)
g1<-graph.data.frame(bactpara.redes2)
intento<-data.frame(g2$nodes)
edge2<-data.frame(g2$edges)
intento$groups<- c('1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '2','2','2','2','2','2','2',
                   '2','2','2','2','2','2','2',
                   '2','2','2','2','2','2','2','2')
net <- graph.data.frame(d=edge2,vertices = intento,directed = T )
class(g1)
g1



plot(net, layout = layout_in_circle,vertex.size=15,
     edge.arrow.size=.1, vertex.label.color="black", edge.curved=0.1,
     edge.color="black",mark.col = T,loop.size = 3,axes = T,
     vertex.label.dist=0,vertex.label.size=1 ) 
legend(x=1.5,y=-0.15, c("Bacteria","Frog"), pch=21,
       col="#777777", pt.bg=c("lightblue","gray50"), pt.cex=3, cex=1.2, bty="n", ncol=1)

intento$color <- c("lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "gray50","gray50","gray50","gray50","gray50","gray50","gray50",
                   "gray50","gray50","gray50","gray50","gray50","gray50","gray50",
                   "gray50","gray50","gray50","gray50","gray50","gray50","gray50", "gray50")


#####Hongos#####

hongoin<-hongos_spp[,c(1,3,4)]
hongoin
library(tidyr)
hongoin2<-unite(hongoin, variables,c(2,1),  sep = "_ ", remove = TRUE)
citation("igraph")
library(igraph) # for Network graph
library(visNetwork)
h1<-graph.data.frame(hongoin2)
h2<-toVisNetworkData(h1)
intento<-data.frame(h2$nodes)
edge2<-data.frame(h2$edges)
intento$groups<- c('1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '1','1','1','1','1',
                   '2','2','2','2','2','2','2',
                   '2','2','2','2','2','2','2',
                   '2','2','2','2','2','2','2','2')
net <- graph.data.frame(d=edge2,vertices = intento,directed = T )
class(g1)
g1



plot(net, layout = layout_in_circle,vertex.size=15,
     edge.arrow.size=.1, vertex.label.color="black", edge.curved=0.1,
     edge.color="black",mark.col = T,loop.size = 3,axes = T,
     vertex.label.dist=0,vertex.label.size=1 ) 
legend(x=1.5,y=-0.15, c("Frog","Funghi"), pch=21,
       col="#777777", pt.bg=c("lightblue","gray50"), pt.cex=3, cex=1.2, bty="n", ncol=1)

intento$color <- c("lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue","lightblue","lightblue","lightblue","lightblue",
                   "lightblue", "lightblue",
                   "gray50","gray50","gray50","gray50","gray50","gray50","gray50",
                   "gray50","gray50","gray50")







