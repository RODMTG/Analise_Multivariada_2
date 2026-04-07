# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 10. Canonical correlation analysis
#
# Example 10.1: Environmental and genetic correlations for colonies of a butterfly
#
# Canonical correlations computed by eigenalysis
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
# Computing linear correlations, separately for each set, and together
# See Table 10.1 on page 188
A <- cor(env.std)
B <- cor(gen.std)
C <- cor(env.std,gen.std)
# Solving the eigenvalue problem defined by Equation 10.1
CC.butterfly <- eigen(solve(B)%*%t(C)%*%solve(A)%*%C)
CC.butterfly
# Displaying the eigenvectors for the standardized Y-variables 
(eigenvec.butterfly.Y <- CC.butterfly$vector)
# Getting the eigenvectors for the standardized X-variables, Equation 10.2
(eigenvec.butterfly.X <- solve(A)%*%C%*%eigenvec.butterfly.Y)
# Displaying the eigenvalues
(eigenvec.butterfly <- CC.butterfly$values)
# Computing the canonical correlations
(ccorr <- sqrt(CC.butterfly$values))
# Bartlett's test
# The null hypothesis is that the first (and smaller) canonical correlations are zero.
n <- nrow(env)
p <- ncol(env)
q <- ncol(gen)
# Bartlett's test statistic as given by Equation 10.3
test.stat <- -( n - 0.5*(p + q + 3) ) * sum(log(1 - CC.butterfly$values))
test.stat
# Computing p-value
P.value <- pchisq(test.stat, df = p*q, lower.tail=FALSE)
P.value
#
# End of script
