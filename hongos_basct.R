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





plot(net)
summary(g1)
 visNetwork(intento, g2$edges, width = "900px",height = "300px") %>% 
  visEdges(arrows = 'to',color = 'black',hoverWidth = 2) %>% 
   visNodes(size = 15,font = "39px tahoma black",physics = F) %>% 
  visHierarchicalLayout(direction = 'LR',treeSpacing = 500,
                        levelSeparation = 1500,nodeSpacing = 400,
                        sortMethod = 'directed',edgeMinimization = F
                            )  %>% 
  visLegend(useGroups = F, addNodes = data.frame(label = c("Bacterias",'Frogs'), shape = c("triangle",'circle'),color=c('lightblue','blue')), 
            addEdges = data.frame(label = "link", color = "black")) %>% 
  visExport()
 
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
               nodesIdSelection = TRUE,manipulation = T ) %>%
    visGroups(useDefaultGroups =T )%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()
}

largest <- largest.cliques(g1)
largest
b.visNetwork(g2,largest,4)

visSave(nodos,file = "network.html")
nodes <- data.frame(id = 1:3, title = paste0("<p>", 1:3,"<br> tooltip</p>"))
edges <- data.frame(from = c(1,2), to = c(1,3))

visNetwork(g2$nodes, g2$edges) %>%
  visConfigure(enabled = TRUE, filter = "interaction")

# using visNetworkEditor
network <- visNetwork(g2$nodes, g2$edges)

custom_network <- visNetworkEditor(object = network)
custom_network
visSave(visNetworkEditor(object = network), file = 'intento.html')

custom_network <- visNetworkEditor(object = network, filter = "edges")
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
hong<-ggplot(hongos_spp,aes(x=Spp_Anuro,fill=Spp_Hongo ))+
  geom_bar(  ) +
  theme_classic( )+
  geom_dotplot(aes( col=Loc  ),method = 'bygroup' )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1 ))


hong
##gridExtra::grid.arrange(bact,hong)

citation("visNetwork")
visNetwork::visDocumentation()

