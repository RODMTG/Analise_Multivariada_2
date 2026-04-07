# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 8. Discriminant function analysis
#
# Example 8.1: Comparison of samples of Egyptian skulls
#
# Linear discriminant analysis, using lda, with allocations to groups
skulls <- read.csv("Egyptian skulls.csv",header=TRUE)
attach(skulls)
# lda is a function of the package MASS
# Before running library(MASS) make sure you have installed the package
library(MASS)
skulls.lda <- lda(Period ~ ., data=skulls)
skulls.lda
# One of the objects in skulls.lda is skulls.lda$scaling, the coefficients of
# linear discriminants, a matrix which transforms observations to discriminant
# functions, normalized so that within groups covariance matrix is spherical
# (similar to equations 8.2, but with a different scaling)
skulls.lda$scaling
# Producing the allocation table (Table 8.4)
alloc.tab <- table(Period,predict(skulls.lda)$class)
alloc.tab[c(2,3,1,4,5),c(2,3,1,4,5)]
#
detach(skulls)
#
# End of script