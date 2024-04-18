bacterias<-bactpara.redes[,-2]

library(igraph) # for Network graph
library(visNetwork) # for interaction Network graph
g1 <- graph.data.frame(bactpara.redes2,directed = T  )
plot(g1,loop.size = 2, vertex.color = "lightblue", layout = layout_with_kk
     ,axes = F, vertex.size=20, vertex.label.dist=1.5,
     edge.arrow.size=1,label.cex=10
      )
summary(g1)
igraph_options(plot.layout=layout_as_tree)
tkplot(g1, vertex.color = "lightblue")
rglplot(g1)



visIgraph(g1) %>%
  visNodes(size = 25, shape = 'hexagon') %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction( multiselect = T,selectable = TRUE,
                  navigationButtons = T,hover = F,
                  hideEdgesOnZoom = F,keyboard = 'speed' ) 

g2 <- toVisNetworkData(g1)
b.visNetwork <- function(g2,largest,number){
  g2$nodes$group<-"single"
  g2$nodes$group[largest[[number]]] <- "clique"
  #Plot
  visNetwork(g2$nodes, g2$edges) %>%
    visIgraphLayout() %>%
    visNodes(size = 25, shape = "circle") %>%
    visOptions(selectedBy="group",
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visGroups(groupname = "clique", color = "orange")%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()%>%
    visNetworkEditor(object = g2, filter = "nodes,edges")
}

largest <- largest.cliques(g1)
largest
nodos<-b.visNetwork(g2,largest,4)

visSave(nodos,file = "network.html")
nodes <- data.frame(id = 1:3, title = paste0("<p>", 1:3,"<br> tooltip</p>"))
edges <- data.frame(from = c(1,2), to = c(1,3))

visNetwork(g2$nodes, g2$edges) %>%
  visConfigure(enabled = TRUE, filter = "interaction")

# using visNetworkEditor
network <- visNetwork(g2$nodes, g2$edges)
custom_network <- visNetworkEditor(object = network)
custom_network
visSave(visNetworkEditor(object = nodos), file = 'intento.html')

custom_network <- visNetworkEditor(object = nodos, filter = "edges")
custom_network

require(graphics)

with(ToothGrowth, {
  interaction.plot(dose, supp, len, fixed = TRUE)
  dose <- ordered(dose)
  interaction.plot(dose, supp, len, fixed = TRUE, col = 2:3, leg.bty = "o")
  interaction.plot(dose, supp, len, fixed = TRUE, col = 2:3, type = "p")
})

with(OrchardSprays, {
  interaction.plot(treatment, rowpos, decrease)
  interaction.plot(rowpos, treatment, decrease, cex.axis = 0.8)
  ## ordena las filas por su efecto medio
  rowpos <- factor(rowpos,
                   levels = sort.list(tapply(decrease, rowpos, mean)))
  interaction.plot(rowpos, treatment, decrease, col = 2:9, lty = 1)
})





library(ggplot2)

bact<-ggplot(bactpara.redes,aes(x=Especie.anuro,fill=BACTERIAS, colour=LOCALIDAD ))+
  geom_bar(  ) +
  theme_classic( )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ))
bact
hong<-ggplot(hongos_spp,aes(x=Spp_Anuro,fill=Spp_Hongo ))+
  geom_bar(  ) +
  theme_classic( )+
  geom_dotplot(aes( col=Loc) )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ))


hong


bact<-ggplot(bactpara.redes2,aes(x=Anuro,fill=Bact ))+
  geom_bar(  ) +
  theme_classic( )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ))
bact
hong<-ggplot(hongos_spp,aes(x=Spp_Anuro,fill=Spp_Hongo ))+
  geom_bar(  ) +
  theme_classic( )+
  geom_dotplot(aes( col=Loc) )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ))


hong







gridExtra::grid.arrange(bact,hong, ncol=1)

