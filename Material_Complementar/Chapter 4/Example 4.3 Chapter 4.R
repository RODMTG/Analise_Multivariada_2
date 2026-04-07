# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 4. Tests of significance with multivariate data
#
# Example 4.3 Comparison of samples of Egyptian skulls
#
skulls <- read.csv("Egyptian skulls.csv",header=TRUE)
#
# One-way ANOVA tests: comparing univariate means
skulls.aovMB <- aov(Maximum.breadth ~ Period, data=skulls)
summary(skulls.aovMB)
skulls.aovBH <- aov(Basibregmatic.height ~ Period, data=skulls)
summary(skulls.aovBH)
skulls.aovBL <- aov(Basialveolar.length ~ Period, data=skulls)
summary(skulls.aovBL)
skulls.aovNH <- aov(Nasal.height ~ Period, data=skulls)
summary(skulls.aovNH) 
#
# Comparing multivariate means (One-way MANOVA)
skulls.mnv <- manova(as.matrix(skulls[,-1])~ Period, data=skulls)
# F-tests after the One-way MANOVA
summary.manova(skulls.mnv) # Pillai's trace statistic is the default in R
summary.manova(skulls.mnv,test="Wilks")
summary.manova(skulls.mnv,test="Hotelling-Lawley")
summary.manova(skulls.mnv,test="Roy")
#
# Box's M test applied to the Egyptian skulls data
# Using function boxM from package biotools
# This function produces a Box's M test based on a chi-square statistic
# Before running library(biotools) make sure you have installed the package
library(biotools)
groups <- skulls[,1] # The grouping variable is located in the 1st column 
vars <- skulls[,-1]  # The y-variables are not located in the 1st column
# Producing the chi-square test of homogeneity of variance-covariance matrices
chitest.boxM <- boxM(vars, groups)
# Now performing Box's M test based on an F statistic (Section 4.8)
n <- nrow(skulls)    ## Number of sampling units 
m <- nlevels(groups) ## Number of groups
p <- ncol(skulls)-1  ## Number of variables
logM <- 0
ni <- rep(NULL,m)
for (i in 1:m) {
     ni[i] <- sum(as.numeric(groups) == i)
     logM <- logM + ((ni[i]-1)/2)*(chitest.boxM$logDet[i])
}
logM <- logM -((n-m)*log(det(chitest.boxM$pooled))/2)
BoxM.stat <- exp(logM)
names(BoxM.stat) <- "Box's M"
BoxM.stat
c1 <- (2*p^2 + 3*p - 1)*(sum(1/(ni-1))-(1/(n-m)))/(6*(p+1)*(m-1))
c2 <- (p-1)*(p+2)*(sum(1/(ni-1)^2)-(1/(n-m)^2))/(6*(m-1))
v1 <- p*(p+1)*(m-1)/2
v2 <- (v1+2)/(c2-c1^2)
b <-  (1 - c1 - v1/v2)/v1
b
b1 <- (1 - c1 - 2/v2)/v2
F.BoxM <- ifelse(c2 > c1^2, -2*b*logM, (-2*b1*v2*logM)/(v1+2*b1*v1*logM))
F.BoxM
Pvalue <- pf(F.BoxM, v1, v2, lower=FALSE)
Pvalue
# End of the script
