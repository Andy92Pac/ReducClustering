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
NbClust(data = coil.data, method = )