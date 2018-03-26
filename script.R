library(R.matlab)
library(FactoMineR)
library(NbClust)

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
kmeans.coil = NbClust(data = coil.data, method = "kmeans")
kmeans.coil.part = kmeans.coil$Best.partition

average.coil =NbClust(data = coil.data, method = "average")
average.coil.part =average.coil$Best.partition

ward.coil = NbClust(data = coil.data, method = "ward.D")
ward.coil.part = ward.coil$Best.partition

single.coil = NbClust(data = coil.data, method = "single")
single.coil.part = single.coil$Best.partition

complete.coil = NbClust(data = coil.data, method = "complete")
complete.coil.part = complete.coil$Best.partition


#5





##########
###jaffe###
##########

jaffe.data <- jaffe$X
jaffe.label <- jaffe$y

# Q3 : ACP
pca.jaffe <- PCA(jaffe.data)

# Q4 : NbClust
kmeans.jaffe = NbClust(data = jaffe.data, method = "kmeans")
kmeans.jaffe.part = kmeans.jaffe$Best.partition

average.jaffe =NbClust(data = jaffe.data, method = "average")
average.jaffe.part =average.jaffe$Best.partition

ward.jaffe = NbClust(data = jaffe.data, method = "ward.D")
ward.jaffe.part = ward.jaffe$Best.partition

single.jaffe = NbClust(data = jaffe.data, method = "single")
single.jaffe.part = single.jaffe$Best.partition

complete.jaffe = NbClust(data = jaffe.data, method = "complete")
complete.jaffe.part = complete.jaffe$Best.partition


#5




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




