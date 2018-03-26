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
