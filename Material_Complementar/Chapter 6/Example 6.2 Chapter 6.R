# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 6. Principal components analysis
#
# Example 6.2 Employment in European countries
#
euroemp <- read.csv("Euroemp.csv",header=TRUE, row.names=1)
attach(euroemp)
# Correlations between the nine industry groups (variables) (Table 6.5)
cor(euroemp[-1])
# Using prcomp to compute the principal components (eigenvalues and eigenvectors)
# With scale=TRUE, variable means are set to zero, and variances set to one
# sample scores stored in euroemp_pca$x
# singular values (square roots of eigenvalues) stored in euroemp_pca$sdev
# loadings (eigenvectors) are stored in euroemp_pca$rotation
# variable means stored in euroemp_pca$center
# variable standard deviations stored in euroemp_pca$scale
euroemp_pca <- prcomp(euroemp[,-1],scale=TRUE)
summary(euroemp_pca)
#
# A table containing eigenvalues and %'s accounted, follows
(eigen_euroemp <- euroemp_pca$sdev^2)            # Eigenvalues are the sdev^2
names(eigen_euroemp) <- paste("PC",1:9,sep="")
eigen_euroemp
sumlambdas <- sum(eigen_euroemp)
propvar <- eigen_euroemp/sumlambdas
cumvar_euroemp <- cumsum(propvar)
matlambdas <- rbind(eigen_euroemp,propvar,cumvar_euroemp)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
# Printing table of eigenvalues and their contribution to the total variance
round(matlambdas,4)
# Showing the eigenvectors    
euroemp_pca$rotation
# The print method below is the same as  euroemp_pca$sdev and 
# euroemp_pca$rotation printed together
print(euroemp_pca)     #
# Sample scores stored in euroemp_pca$x
euroemp_pca$x
# Identifying the scores by country
eurotyp_pca <- data.frame(euroemp_pca$x)
eurotyp_pca
# Plotting the scores for the first and second components
# This plot is similar to Figure 6.2
plot(eurotyp_pca$PC1, eurotyp_pca$PC2, type="n", 
     xlab="PC1", ylab="PC2", main="Figure 6.2 (signs reversed)")
text(eurotyp_pca$PC1, eurotyp_pca$PC2,labels=rownames(euroemp), cex=0.5)
abline(h=0)
abline(v=0)
#
detach(euroemp)
# End of script
