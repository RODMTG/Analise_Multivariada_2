# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.1: Plant species in the Steneryd Nature Reserve
#
# Ordination by PCA
steneryd.read <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)
library(vegan)
steneryd1 <- data.frame(steneryd.read[,2:18])
steneryd <- data.frame(t(data.frame(steneryd1)))
colnames(steneryd) <- steneryd.read[,1]
rownames(steneryd) <- as.character(1:17)
steneryd
attach(steneryd)
# Using prcomp command (Chapter 6)
steneryd.pca <- prcomp(steneryd,scale=TRUE)
summary(steneryd.pca)                                                 
eigen.steneryd <- steneryd.pca$sdev^2          # Eigenvalues are the sdev^2
sum.lambdas <- sum(eigen.steneryd)
perc.var <- (eigen.steneryd/sum.lambdas)*100
cumvar.steneryd <- cumsum(perc.var)
matlambdas <- cbind(c(1:17),eigen.steneryd,perc.var,cumvar.steneryd)
colnames(matlambdas) <- c("Component","Eigenvalue","% of Total","Cumulative %")
# Printing Table 12.1 (non-zero eigenvalues; the last row in Table 12.1 is not displayed)
# Eigenvalues and their contribution to the total variance
(mateigen.csv <- round(matlambdas[-17,],2))            
write.csv(mateigen.csv,"Eigenvalues Steneryd plants.csv",row.names=FALSE)
# The 17 principal components for the plant data
PCs.steneryd <- steneryd.pca$rotation
# The first three principal components for the plant data (similar to Table 12.2)          
PCs.steneryd[,1:3]
First3PCs.steneryd <- write.csv(PCs.steneryd[,1:3],"First 3PCs Steneryd.csv")
# Sample scores stored in steneryd.pca$x
steneryd.pca$x
# Plotting the first 3 PCs for the 17 plots and plot number (similar to Figure 12.1)
# Using splom from the lattice package
library(lattice)
mat.plotPCs <- cbind(as.numeric(rownames(steneryd)),steneryd.pca$x[,1:3])
colnames(mat.plotPCs) <- c("Plot",colnames(steneryd.pca$x[,1:3]))
splom(mat.plotPCs,pch=16,col="red")
#
detach(steneryd)
#
# End of script
