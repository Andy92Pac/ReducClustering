library(R.matlab)
library(FactoMineR)
library(NbClust)
library(kernlab)
library(Rmixmod)
library(mclust)
library(factoextra)
library(corrplot)
library(cluster)

coil <- readMat("data/DATA_MATLAB - Projet-master-MLDS/COIL20_1440n_1024d_20c.mat")
jaffe <- readMat("data/DATA_MATLAB - Projet-master-MLDS/jaffe.mat")
mnist <- readMat("data/DATA_MATLAB - Projet-master-MLDS/MNIST5.mat")
mfeat <- readMat("data/DATA_MATLAB - Projet-master-MLDS/MFEAT1.mat")

##########
###coil###
##########

coil.data <- coil$X
table(jaffe.label)
coil.label <- coil$y

# Q3 : ACP
pca.coil <- PCA(coil.data,scale.unit = TRUE)
fviz_eig(pca.coil, addlabels = TRUE, ylim = c(0, 50),main = "Graphique des valeurs propres cumulées par dimensions")

fviz_pca_ind (pca.coil, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

fviz_pca_ind (pca.coil)

fviz_cos2(pca.coil, choice = "ind")

#Contribution totale sur Dim 1 et 2
fviz_contrib(pca.coil, choice = "ind", axes = 1:2)

head(corrplot(pca.coil$var$cos2,is.corr = FALSE), 4)

silhouette(kmeans.coil)


# Q4 : NbClust
#Cluster normal
kmeans.coil = NbClust(data = coil.data, method = "kmeans",min.nc = 10,max.nc = 20)
kmeansCoilBestPart3Index<- kmeans.coil$Best.partition

#Best Cluster 20

average.coil =NbClust(data = coil.data, method = "average",min.nc = 10,max.nc = 20)
View(complete.coil$Best.nc)
average.coil.part =average.coil$Best.partition

View(average.coil$Best.nc)

ward.coil = NbClust(data = coil.data, method = "ward.D",min.nc = 10,max.nc = 20)
ward.coil.part = ward.coil$Best.partition

single.coil = NbClust(data = coil.data, method = "single",min.nc = 2,max.nc = 20)
single.coil.part = single.coil$Best.partition

complete.coil = NbClust(data = coil.data, method = "complete",min.nc = 10,max.nc = 20)
complete.coil.part = complete.coil$Best.partition


#5
hcpc.coil <- HCPC(pca.coil,nb.clust = -1,graph = TRUE)
hcpc.coil$desc.ind

fviz_dend(hcpc.coil, 
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco"           # Couleur du rectangle
)

fviz_cluster(hcpc.coil,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

#6
n.cluster.coil = 5
spect.coil = specc(x = coil.data, n.cluster.coil)

#7




#8
# mclust
mclust.coil = Mclust(coil.data)
# mixmod
mixmodCluster(as.data.frame(coil.data), 3:10, models = mixmodMultinomialModel("Binary_pk_Ekj"))

#9
mclustdr.coil = MclustDR(mclust.coil)
plot(mclustdr.coil)



##########
###jaffe###
##########

jaffe.data <- jaffe$X
jaffe.label <- jaffe$y

# Q3 : ACP
pca.jaffe <- PCA(jaffe.data)

fviz_eig(pca.jaffe, addlabels = TRUE, ylim = c(0, 50),main = "Graphique des valeurs propres cumulées par dimensions")

fviz_pca_ind (pca.jaffe, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

fviz_pca_ind(pca.jaffe,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = jaffe.label, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)

fviz_pca_ind (pca.jaffe)

fviz_cos2(pca.jaffe, choice = "ind")

#Contribution totale sur Dim 1 et 2
fviz_contrib(pca.coil, choice = "ind", axes = 1:2)

#10 classes
kmeans.jaffe = NbClust(jaffe$X, method = "kmeans",min.nc = 2,max.nc = 10,index = "silhouette")
kmeans.jaffe$Best.nc
kmeans.jaffe.part 

#9 classes
average.jaffe =NbClust(data = jaffe.data, method = "average",min.nc = 2,max.nc = 10,index = "silhouette")
average.jaffe$Best.nc
average.jaffe.part =average.jaffe$Best.partition
average.jaffe.part

#10 classes
ward.jaffe = NbClust(data = jaffe.data, method = "ward.D",min.nc = 2,max.nc = 10,index = "silhouette")
ward.jaffe.part = ward.jaffe$Best.partition
ward.jaffe$Best.nc


#7 classes
single.jaffe = NbClust(data = jaffe.data, method = "single",min.nc = 2,max.nc = 10,index = "silhouette")
single.jaffe.part = single.jaffe$Best.partition
single.jaffe.part
single.jaffe$Best.nc

#6 classes
complete.jaffe = NbClust(data = jaffe.data, method = "complete",min.nc = 2,max.nc = 10,index = "silhouette")
complete.jaffe.part = complete.jaffe$Best.partition
complete.jaffe$Best.nc


#5
hcpc.jaffe <- HCPC(pca.jaffe,nb.clust = -1,graph = TRUE)

fviz_dend(hcpc.jaffe, 
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco"           # Couleur du rectangle
)

fviz_cluster(hcpc.jaffe,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

#6
n.cluster.jaffe = 10
spect.jaffe = specc(x = jaffe.data, n.cluster.jaffe)
plot(jaffe$X, col = spect.jaffe)
#7




#8
# mclust
mclust.jaffe = Mclust(jaffe.data)
mclust.jaffe
# mixmod
mixmodCluster(as.factor(jaffe.data), 3:10, models = mixmodMultinomialModel("Binary_pk_Ekj"))

#9
mclustdr.jaffe = MclustDR(mclust.jaffe)
plot(mclustdr.jaffe)


##########
###mnist###
##########

mnist.data <- mnist$X
mnist.label <- mnist$y

# Q3 : ACP
pca.mnist <- PCA(mnist.data)

# Q4 : NbClust
kmeans.mnist = NbClust(data = mnist.data, method = "kmeans")
kmeans.mnist.part = kmeans.mnist$Best.partition

average.mnist =NbClust(data = mnist.data, method = "average")
average.mnist.part =average.mnist$Best.partition

ward.mnist = NbClust(data = mnist.data, method = "ward.D")
ward.mnist.part = ward.mnist$Best.partition

single.mnist = NbClust(data = mnist.data, method = "single")
single.mnist.part = single.mnist$Best.partition

complete.mnist = NbClust(data = mnist.data, method = "complete")
complete.mnist.part = complete.mnist$Best.partition


#5
HCPC(pca.mnist)

#6
n.cluster.mnist = 9
spect.mnist = specc(x = mnist.data, n.cluster.mnist)

#7



#8
# mclust
mclust.mnist = Mclust(mnist.data)
# mixmod
mixmodCluster(mnist.data, 3:10, dataType = "quantitative", models = mixmodMultinomialModel("Binary_pk_Ekj"))

#9
mclustdr.mnist = MclustDR(mclust.mnist)
plot(mclustdr.mnist)


##########
###mfeat###
##########

mfeat.data <- mfeat$X
mfeat.label <- mfeat$y

# Q3 : ACP
pca.mfeat <- PCA(mfeat.data)

# Q4 : NbClust
kmeans.mfeat = NbClust(data = mfeat.data, method = "kmeans")
kmeans.mfeat.part = kmeans.mfeat$Best.partition

average.mfeat =NbClust(data = mfeat.data, method = "average")
average.mfeat.part =average.mfeat$Best.partition

ward.mfeat = NbClust(data = mfeat.data, method = "ward.D")
ward.mfeat.part = ward.mfeat$Best.partition

single.mfeat = NbClust(data = mfeat.data, method = "single")
single.mfeat.part = single.mfeat$Best.partition

complete.mfeat = NbClust(data = mfeat.data, method = "complete")
complete.mfeat.part = complete.mfeat$Best.partition


#5
HCPC(pca.mfeat)

#6
n.cluster.mfeat = 9
spect.mfeat = specc(x = mnist.data, n.cluster.mfeat)

#7
table(as.data.frame(coil.label))

#8
# mclust
mclust.mfeat = Mclust(mfeat.data)
# mixmod
mixmodCluster(mfeat.data, 3:10, dataType = "quantitative", models = mixmodMultinomialModel("Binary_pk_Ekj"))

#9
mclustdr.mfeat = MclustDR(mclust.mfeat)
plot(mclustdr.mfeat)









