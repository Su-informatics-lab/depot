
#' ---------------------------
#'  step 4: check embedding  |
#' ---------------------------
#' (1) load prior data
feat.new <- readRDS(file='./input_data/feat.new.RDS')

#' (2) load embedding
p1 <- './GraphSage_model/output/'
val1 <- read.csv(paste0(p1,"embedding_val.csv"),header=F,sep=',')
id1 <- read.csv(paste0(p1,"val_value.txt"),header=F,sep=' ')

#' @@@ match index !!!
#' @param feat.new
id2 <- id1
id2[,1] <- id2[,1]+1
po <- match(rownames(feat.new),id2[,1])
val2 <- val1[po,]
rownames(val2) <- 1:nrow(val2)
#' @param val2

#' --------- umap of embedding ------------
library(ggplot2); library(umap); 

#' umap
set.seed(123); umap.ckd = umap(as.matrix(val2))
embedding.umap <- as.data.frame(umap.ckd$layout)

embedding.umap$age  <- feat.new[,37] #' age
embedding.umap$egfr  <- feat.new[,15]   #' egfr

embeddings <- embedding.umap

library(ggplot2); source('visualize.utility.R')

p1 <- ggplot(embeddings, aes(x=V1, y=V2, color=age)) +
    geom_point(size=0.1) +
    scale_color_gradientn(colours = rainbow(2))+
    xlab("UMAP1") + ylab("UMAP2") +
    ggtitle("Age: UMAP of embedding") +
    ckd_theme_opts() 

tem.egfr <- embeddings#' [embeddings$egfr!=0,]

p3 <- ggplot(tem.egfr, aes(x=V1, y=V2, color=egfr)) +
    geom_point(size=0.1) +
    xlab("UMAP1") + ylab("UMAP2") +
    ggtitle("EGFR: UMAP of embedding") +
    ckd_theme_opts() + scale_color_gradient2(midpoint=0, low="blue", mid="white",
                                             high="red", space ="Lab" )

ggsave("./result_figures/umap_age.png", p1, width=8, height=6, units="in")
ggsave("./result_figures/umap_egfr.png", p3, width=8, height=6, units="in")

#' ---------------------
#'  plot 3D DDR tree   |
#' ---------------------
library(DDRTree)

DDRTree_res <- DDRTree(t(as.matrix(val2)), dimensions = 3, maxIter = 20, sigma = 1e-3, lambda = 1000, ncenter = 1000, param.gamma = 5, tol = 1e-2, verbose = FALSE)

library(DDRTree); 
library(ggrepel); library(plot3D)

Z <- DDRTree_res$Z 
Y <- DDRTree_res$Y

#' ---------------
#' @param ref.tree ; Z
#' @param ref.backbone ; Y
library(ggrepel); library(plot3D)
ref.tree <- data.frame(t(DDRTree_res$Z)); ref.tree$age <- feat.new[,37]  #' age 

#tem <- feat.new[,15]; # plot scaled value or raw value
ref.tree$egfr <- feat.new[,15]

#' @param ref.tree ; Z
#' @param ref.backbone ; Y

ref.backbone <- data.frame(t(Y)); colnames(ref.backbone) <- c('Y1','Y2','Y3')

xlim_init <- c(min(ref.tree$X1),max(ref.tree$X1))
ylim_init <- c(min(ref.tree$X2),max(ref.tree$X2))
zlim_init <- c(min(ref.tree$X3),max(ref.tree$X3))

#' --------------------------------------
#'  clustering of encounters on DDRtree |
#' --------------------------------------
library(fpc); library(dbscan)

set.seed(123); km.res <- kmeans(ref.tree[,1:3], 30, nstart = 25)
ref.tree$km_cluster <- km.res$cluster

centroids <- sapply(1:30,function(i){
     return (which.min(rowSums(abs(km.res$centers[i,]-ref.tree[,1:3])))) })

tem <- split(ref.tree,ref.tree$km_cluster)
ps.age <- order(sapply(1:30,function(i){median(tem[[i]]$age)}))

ref.tree$cluster_order_age <- factor(ref.tree$km_cluster,levels=ps.age)

library(plyr);
median.age <- sapply(1:30,function(i){median(tem[[i]]$age)})
ref.tree$median_age <- mapvalues(ref.tree$km_cluster,1:30,median.age)

median.egfr <- sapply(1:30,function(i){median(tem[[i]]$egfr)})
ref.tree$median_egfr <- mapvalues(ref.tree$km_cluster,1:30,median.egfr)

#' ------ plot age in each cluster of trajecotry  -------
col1 <- ramp.col(c('blue','white','red'))

png(file='./result_figures/plot3D_trajectory.png',width=2100,height=2100,res=300)
scatter3D(ref.tree$X1,ref.tree$X2,ref.tree$X3,
          colvar = ref.tree$median_egfr,
          pch=20,bty='g',cex = 0.5,phi=10,theta=85,surface=FALSE,col=col1,
          xlim = xlim_init, ylim = ylim_init, zlim = zlim_init)
scatter3D(ref.backbone$Y1,ref.backbone$Y2,ref.backbone$Y3,pch=20,
          add = T,cex = 0.7,col='#000000',phi=10,theta=85,surface=FALSE,
          xlim = xlim_init, ylim = ylim_init, zlim = zlim_init)
graphics.off()


png(file='./result_figures/plot3D_trajectory_age.png',width=2100,height=2100,res=300)
scatter3D(ref.tree$X1,ref.tree$X2,ref.tree$X3,colvar = as.numeric(ref.tree$median_age),
          pch=20,bty='g',cex = 0.5,phi=10,theta=85,surface=FALSE,col=col1,
          xlim = xlim_init, ylim = ylim_init, zlim = zlim_init)
scatter3D(ref.backbone$Y1,ref.backbone$Y2,ref.backbone$Y3,pch=20,
          add = T,cex = 0.7,col='#000000',phi=10,theta=85,surface=FALSE,
          xlim = xlim_init, ylim = ylim_init, zlim = zlim_init)
graphics.off()

