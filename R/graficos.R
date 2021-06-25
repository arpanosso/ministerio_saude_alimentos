my_biplot <- function(da,rotulos,titulo){
  pca <-  prcomp(da,scale.=T)
  eig<-pca$sdev^2
  ve<-eig/sum(eig)
  pc1V<-cor(da,pca$x)[,1]/sd(cor(da,pca$x)[,1])
  pc2V<-cor(da,pca$x)[,2]/sd(cor(da,pca$x)[,2])
  pc1c<-pca$x[,1]/sd(pca$x[,1])
  pc2c<-pca$x[,2]/sd(pca$x[,2])
  Agrupamento<-as.factor(rotulos)
  bip<-data.frame(pc1c,pc2c,Agrupamento)
  texto <- data.frame(
    x = pc1V,
    y = pc2V,
    label = names(da))
  
  p<-bip %>% ggplot(aes(x=pc1c,y=pc2c))+
    geom_point(aes(color = Agrupamento)) + 
    theme_minimal() +
    geom_vline(aes(xintercept=0),
               color="black", size=1) +
    geom_hline(aes(yintercept=0),
               color="black", size=1) +
    annotate(geom="segment",
             x=rep(0,length(da)),
             xend=texto$x,
             y=rep(0,length(da)),
             yend=texto$y,color="black",lwd=.5) +
    geom_label(data=texto,aes(x=x,y=y,label=label),
               color="black",angle=0,fontface="bold",size=4,fill="white") +
    labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
         y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
         title = titulo) +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}
