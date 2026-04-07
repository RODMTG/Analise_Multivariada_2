# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.2: Burials in Bannadi
#
# Ordination by PCA
burials <- read.csv("Bannadi burials.csv",header=TRUE)
summary(burials[,1])
# Using prcomp command (Chapter 6)
burials.pca <- prcomp(burials[3:40], scale=TRUE)
summary(burials.pca)                                                 
eigen.burials <- burials.pca$sdev^2          # Eigenvalues are the sdev^2
sum.lambdas <- sum(eigen.burials)
perc.var <- (eigen.burials/sum.lambdas)*100
cumvar.burials <- cumsum(perc.var)
matlambdas <- cbind(c(1:38),eigen.burials,perc.var,cumvar.burials)
colnames(matlambdas) <- c("Component","Eigenvalue","% of Total","Cumulative %")
(mateigen.csv <- round(matlambdas,2))           # Printing table of eigenvalues and their contribution to the total variance
write.csv(mateigen.csv,"Eigenvalues Bannadi burials.csv",row.names=FALSE)
PCs.burials <- burials.pca$rotation          # The 38 principal components for the burial data
# The first four principal components for the burial data (Table 12.3)          
First4PCs.burials <- PCs.burials[,1:4]
rownames(First4PCs.burials)<- 1:38
First4PCs.burials <- round(First4PCs.burials, 2)
First4PCs.burials
write.csv(First4PCs.burials,"First 4PCs burials.csv",row.names=FALSE)
# Total number of goods
totno.goods <- rowSums(burials[3:40])
# Sample scores stored in sparrow_pca$x
burials.pca$x
# Plotting the first 4 PCs for the 47 Bannadi graves, the total number of
# goods and the type of remains
library(lattice)
mat.plotPCs=cbind(totno.goods,burials[,2],burials.pca$x[,1:4])
colnames(mat.plotPCs)=c("No. goods","Type",colnames(burials.pca$x[,1:4]))
splom(mat.plotPCs,pch=16,col="red")
#
# End of script
#
