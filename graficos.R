library(ggplot2)
#####Bacterias####
ggplot(bactpara.redes2, aes(  x=as.factor(Bact_), fill=as.factor(Anuro)  ))+
  geom_bar()+
  labs(y=" ",x="Bacterias")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
######hongos#######

fix(hongos_spp)
ggplot(hongos_spp, aes(  x=as.factor(Spp_Hongo), fill=as.factor(Spp_Anuro)))+
  geom_bar()+
  labs(y=" ",x="Hongos")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
a
a

