# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 10. Canonical correlation analysis
#
# Example 10.1: Soil and vegetation variables in Belize
#
Belize <- read.csv("Soil and Vegetation Belize.csv", header=TRUE)
soil <- Belize[,1:4]
veg <- Belize[,5:8]
soil.std <- scale(soil)
veg.std <- scale(veg)
#
# Canonical correlations computed by eigenalysis
A <- cor(soil.std)
B <- cor(veg.std)
C <- cor(soil.std,veg.std)
CC.Belize <- eigen(solve(B)%*%t(C)%*%solve(A)%*%C)
(ccorr <- sqrt(CC.Belize$values))
# Bartlett's test
# The null hypothesis is that the first (and smaller) canonical correlations are zero.
n <- nrow(soil)
p <- ncol(soil)
q <- ncol(veg)
test.stat <- -( n - 0.5*(p + q + 3) ) * sum(log(1 - CC.Belize$values))
test.stat
P.value <- pchisq(test.stat, df = p*q, lower.tail=F)
P.value
#
# Canonical correlation analysis with candisc package 
library(candisc)
CC.Belize.candisc <- cancor(soil.std,veg.std, set.names=c("Soil","Vegetation"))
# Wilk's lambda is the test statistic for the decreasing set of canonical variables
# (cancor does not have Bartlett's test)
summary(CC.Belize.candisc)
# Function coef reads the object produced by cancor and produces the canonical
# variate coefficients shown on page 195 (signs reversed)
coef(CC.Belize.candisc, type="both", standardize=TRUE)
# The object "structure" contains the correlations between the environmental
# variables and the canonical variables U1,..., U4, and between the genetic 
# variables and the canonical variables V1,..., V4. See Table 10.3.
CC.Belize.candisc$structure
# Producing Figure 10.2 (Draftman's plot; using parameters horInd and verInd
# in the pairs function)
scores.CC <- CC.Belize.candisc$scores
Case <- 1:nrow(scores.CC$X)
CVS <- data.frame(Case,scores.CC$X,scores.CC$Y)
pairs(CVS,horInd=c(1,2:5),verInd=c(1,6:9), row1attop=FALSE)
mtext(expression(U[1]),side=1,line=-1.3, outer=TRUE, at=c(0.33,-1))
mtext(expression(U[2]),side=1,line=-1.3, outer=TRUE, at=c(0.50,-1))
mtext(expression(U[3]),side=1,line=-1.3, outer=TRUE, at=c(0.67,-1))
mtext(expression(U[4]),side=1,line=-1.3, outer=TRUE, at=c(0.84,-1))
mtext(expression(V[1]),side=2,line=-1.3, outer=TRUE, at=c(-1,0.33), las=2)
mtext(expression(V[2]),side=2,line=-1.3, outer=TRUE, at=c(-1,0.50), las=2)
mtext(expression(V[3]),side=2,line=-1.3, outer=TRUE, at=c(-1,0.67), las=2)
mtext(expression(V[4]),side=2,line=-1.3, outer=TRUE, at=c(-1,0.84), las=2)
#
# Canonical correlation analysis using cca from the pacakge yacca
# (Yet Another Canonical Correlation Analysis) 
# Standardization is specified as arguments xcale= and yscale= in the function cca
# Before running library(yacca) make sure you have installed the package
library(yacca)
CC.yacca <- cca(soil, veg, xscale=TRUE, yscale=TRUE)
# Bartlett test is given by default in yacca, including the extension that 
# tests the importance of each canonical correlation, as discussed on page 185
summary(CC.yacca) # The canonical variate coefficients match those shown on page 195
#                   Possibly differing only in signs
#
# End of script
#