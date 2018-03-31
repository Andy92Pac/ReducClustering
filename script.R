library(R.matlab)
library(FactoMineR)
library(NbClust)
library(kernlab)
library(Rmixmod)
library(mclust)

coil <- readMat("data/DATA_MATLAB - Projet-master-MLDS/COIL20_1440n_1024d_20c.mat")
jaffe <- readMat("data/DATA_MATLAB - Projet-master-MLDS/jaffe.mat")
mnist <- readMat("data/DATA_MATLAB - Projet-master-MLDS/MNIST5.mat")
mfeat <- readMat("data/DATA_MATLAB - Projet-master-MLDS/MFEAT1.mat")

##########
###coil###
##########

coil.data <- coil$X
coil.label <- coil$y

# Q3 : ACP
pca.coil <- PCA(coil.data)

# Q4 : NbClust
kmeans.coil = NbClust(data = coil.data, method = "kmeans",min.nc = 5,max.nc = 10)
kmeans.coil.part = kmeans.coil$Best.partition

average.coil =NbClust(data = coil.data, method = "average",min.nc = 5,max.nc = 10)
average.coil.part =average.coil$Best.partition

ward.coil = NbClust(data = coil.data, method = "ward.D",min.nc = 5,max.nc = 10)
ward.coil.part = ward.coil$Best.partition

single.coil = NbClust(data = coil.data, method = "single",min.nc = 5,max.nc = 10)
single.coil.part = single.coil$Best.partition

complete.coil = NbClust(data = coil.data, method = "complete",min.nc = 5,max.nc = 10)
complete.coil.part = complete.coil$Best.partition


#5
HCPC(pca.coil)

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

#10 classes
kmeans.jaffe = NbClust(jaffe$X, method = "kmeans",min.nc = 5,max.nc = 10,index = "silhouette")
kmeans.jaffe.part = kmeans.jaffe$Best.partition
kmeans.jaffe.part 

#9 classes
average.jaffe =NbClust(data = jaffe.data, method = "average",min.nc = 5,max.nc = 10,index = "silhouette")
average.jaffe.part =average.jaffe$Best.partition
average.jaffe.part

#10 classes
ward.jaffe = NbClust(data = jaffe.data, method = "ward.D",min.nc = 5,max.nc = 10,index = "silhouette")
ward.jaffe.part = ward.jaffe$Best.partition
ward.jaffe.part


#7 classes
single.jaffe = NbClust(data = jaffe.data, method = "single",min.nc = 5,max.nc = 10,index = "silhouette")
single.jaffe.part = single.jaffe$Best.partition
single.jaffe.part

#6 classes
complete.jaffe = NbClust(data = jaffe.data, method = "complete",min.nc = 5,max.nc = 10,index = "silhouette")
complete.jaffe.part = complete.jaffe$Best.partition
complete.jaffe.part


#5
HCPC(pca.jaffe)

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



#8
# mclust
mclust.mfeat = Mclust(mfeat.data)
# mixmod
mixmodCluster(mfeat.data, 3:10, dataType = "quantitative", models = mixmodMultinomialModel("Binary_pk_Ekj"))

#9
mclustdr.mfeat = MclustDR(mclust.mfeat)
plot(mclustdr.mfeat)









