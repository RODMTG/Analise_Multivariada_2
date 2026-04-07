# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 7. Factor analysis
#
# Example 7.1: Employment in European countries
#
# Principal components factor analysis in R
# Using library psych
# Before running library(psych) make sure you have installed the package
library(psych)
euroemp <- read.csv("Euroemp.csv", header=TRUE, row.names=1)
attach(euroemp)
# Applying "principal" from package psych
fit.pc <- principal(euroemp[-1], nfactors=4, rotate="varimax")
fit.pc                  # print results (see page 131)
round(fit.pc$values, 3) # Eigenvalues   (see Table 7.2)
fit.pc$loadings         # Loadings      (this is part of fit.pc)  
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}   # Loadings with more digits
fit.pc$communality      # Communalities (see page 129)
# Rotated factor scores, Table 7.1 (Notice the columns ordering: RC1, RC3, RC2 and RC4)
fit.pc$scores           
#
detach(euroemp)
#
# End of script
