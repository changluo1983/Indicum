# PCA analysis of indicum metabolites 
meta <-read.csv("data/indicum_meta.csv") 
  
  
colnames(meta)

boxplot(meta[,3:9])

meta_scale <- scale(meta[-c(40,57,110,111),3:9])

boxplot(meta_scale)
# hclust of meta
meta_dist <- dist(meta_scale)
clust <- hclust(meta_dist)

plot(clust)

rect.hclust(clust,k=3)

class <- cutree(clust,k=3)



boxplot(meta_scale)

Indicum_name <- meta$ID

which(Indicum_name%in% c("Taiju","Jinsihuangju","Xueju"))

colors <- character(nrow(meta))

colors <- ifelse(Indicum_name%in% c("Taiju","Jinsihuangju","Xueju"),
                 "red","blue")

pca <- prcomp(meta_scale)

plot(pca)

plot(pca$x[,1],pca$x[,2],col=class,cex.axis=2,pch=19,cex=2)

biplot(pca,choices =1:2)

scale(meta[2,3:9])

kmeans(meta_scale,4)

meta$Apigenin <- sort(meta$Apigenin)



barplot(meta$Apigenin,col = colors)
