# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 10. Canonical correlation analysis
#
# Example 10.1: Environmental and genetic correlations for colonies of a butterfly
#
# Canonical correlations computed with cancor from default package stats
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
# Standardizing both sets of variables
env.std <- scale(env)
gen.std <- scale(gen)
# Canonical correlation analysis with cancor
# Standardized variables are used; no centering
CC.butterfly.cancor <- cancor(env.std, gen.std, xcenter=FALSE, ycenter=FALSE)
CC.butterfly.cancor
#
# End of script

