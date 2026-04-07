# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 7. Factor analysis
#
# Example 7.1: Employment in European countries
#
# Principal components factor analysis in R
# Calculations "by hand"
#
euroemp <- read.csv("Euroemp.csv", header=TRUE, row.names=1)
attach(euroemp)
# Computing the correlation matrix (see page 130)
corrm.emp <- cor(euroemp[-1])
corrm.emp
euroemp_pca <- prcomp(euroemp[-1],scale=TRUE)
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
# See Table 7.2
round(matlambdas,4)
# Showing the eigenvectors (Table 7.2)    
eigvec.emp <- euroemp_pca$rotation
eigvec.emp
# The print method below is the same as  euroemp_pca$sdev and 
# euroemp_pca$rotation printed together
print(euroemp_pca) 
# Taking the first four PCs to generate linear combinations for all the
# variables with four factors
pcafactors.emp <- eigvec.emp[,1:4]
# Multiplying each column of the eigenvectors matrix by the
# square-root of the corresponding eigenvalue in order to get
# the factor loadings (see page 129)
unrot.fact.emp <- sweep(pcafactors.emp,MARGIN=2,euroemp_pca$sdev[1:4],`*`)
unrot.fact.emp
# Computing communalities
communalities.emp <- rowSums(unrot.fact.emp^2)
communalities.emp
# Performing the varimax rotation. The default in the varimax function 
# is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.emp <- varimax(unrot.fact.emp)
rot.fact.emp
# The print method of varimax omits loadings less than abs(0.1)
# In order to display all the loadings, it is necessary to ask explicitly 
# the contents of the object $loadings
# Notice that signs are reversed for factors F2 (PC2), F3 (PC3) and F4 (PC4)
# in comparison to the equations on page 131
fact.load.emp <- rot.fact.emp$loadings[1:9,1:4]
fact.load.emp
# Computing the rotated factor scores for the 30 European Countries (Table 7.3)
# Notice that signs are reversed for factors F2 (PC2), F3 (PC3) and F4 (PC4)
scale.emp <- scale(euroemp[-1])
# Calculations based on Equation 7.4 follows
as.matrix(scale.emp)%*%fact.load.emp%*%solve(t(fact.load.emp)%*%fact.load.emp)
#
detach(euroemp)
#
# End of script
#