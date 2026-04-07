# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 10. Canonical correlation analysis
#
# Example 10.1: Environmental and genetic correlations for colonies of a butterfly
#
# Canonical correlations computed with cancor from package candisc
butterfly <- read.csv("Butterfly colonies.csv", header=TRUE, row.names=1)
# The first four variables in data frame "butterfly" are the X's variables
# These are environmental variables
env <- butterfly[,1:4]
# The first Y variable combines 0.40 and 0.60 mobility genes
PFPGi0.40_0.60 <- butterfly[,5] + butterfly[,6]
# The previous combined variables and the last three variables determine the Y's
# These are genetic variables
gen <- cbind(PFPGi0.40_0.60, butterfly[7:9])
# Standardizing both sets of variables
env.std <- scale(env)
gen.std <- scale(gen)
# Canonical correlation analysis with candisc package
# Before running library(candisc) make sure you have installed the package 
library(candisc)
CC.butterfly.candisc <- cancor(env.std,gen.std, set.names=c("Environmental","Genetic"))
# Wilk's lambda is the test statistic for the decreasing set of canonical variables
# (cancor does not have Bartlett's test)
summary(CC.butterfly.candisc)
# Function coef reads the object produced by cancor and produces the canonical
# variate coefficients shown on page 188
coef(CC.butterfly.candisc, type="both", standardize=TRUE)
# The object "structure" contains the correlations between the environmental
# variables and the canonical variables U1,..., U4, and between the genetic 
# variables and the canonical variables V1,..., V4. The interpretation of these
# correlations are discussed on page 189
CC.butterfly.candisc$structure
# Producing Figure 10.1
plot(CC.butterfly.candisc, ellipse=FALSE, abline=FALSE, col="blue", pch=16)
abline(h=0, lty="dashed")
abline(v=0, lty="dashed")
scores.CC <- CC.butterfly.candisc$scores
U1 <- scores.CC$X[,1]
V1 <- scores.CC$Y[,1]
# The points are labeled interactively with the mouse
identify(U1,V1,labels=rownames(gen), cex=0.6)
#
# End of script

