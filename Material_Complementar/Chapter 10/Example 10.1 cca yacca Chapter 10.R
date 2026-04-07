# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 10. Canonical correlation analysis
#
# Example 10.1: Environmental and genetic correlations for colonies of a butterfly
#
# Canonical correlations computed with cca from package yacca
#
butterfly <- read.csv("Butterfly colonies.csv", header=TRUE, row.names=1)
# The first four variables in data frame "butterfly" are the X's variables
# These are environmental variables
env <- butterfly[,1:4]
# The first Y variable combines 0.40 and 0.60 mobility genes
PFPGi0.40_0.60 <- butterfly[,5] + butterfly[,6]
# The previous combined variables and the last three variables determine the Y's
# These are genetic variables
gen <- cbind(PFPGi0.40_0.60, butterfly[7:9])
# Canonical correlation analysis using cca from the pacakge yacca
# (Yet Another Canonical Correlation Analysis) 
# Standardization is specified as arguments xcale= and yscale= in the function cca
# Before running library(yacca) make sure you have installed the package
library(yacca)
CC.yacca <- cca(env, gen, xscale=TRUE, yscale=TRUE)
# Bartlett test is given by default in yacca, including the extension that 
# tests the importance of each canonical correlation, as discussed on page 185
summary(CC.yacca) # The canonical variate coefficients match those shown on page 188
#
# End of script
