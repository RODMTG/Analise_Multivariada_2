# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 8. Discriminant function analysis
#
# Example 8.1: Comparison of samples of Egyptian skulls
#
# Linear discriminant analysis, using lda, with cross-validation
skulls <- read.csv("Egyptian skulls.csv",header=TRUE)
attach(skulls)
# lda is a function of the package MASS
# Before running library(MASS) make sure you have installed the package
library(MASS)
skulls.lda.CV <- lda(Period ~ ., data=skulls, CV=TRUE)
skulls.lda <- lda(Period ~ ., data=skulls)
table(Period,predict(skulls.lda)$class)
# function predict IS NOT USED, as the object (list)produced by lda  
# contains the jackknife classification of individuals: it is the vector class
Alloc_skulls.group <- skulls.lda.CV$class
(table_skulls.alloc <- cbind(Period,data.frame(Alloc_skulls.group)))      # Data frame
(skulls.table <- table(Period,Alloc_skulls.group))                        # Summary of allocations
#
detach(skulls)
#
# End of script
