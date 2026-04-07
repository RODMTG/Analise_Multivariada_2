# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 8. Discriminant function analysis
#
# Example 8.1: Comparison of samples of Egyptian skulls
#
# In order to run the discriminant analysis based on the eigenanalysis of
# (W^(-1))*B, a One-way MANOVA test is performed first, using command "manova"
# and summary.manova (which produces W and B matrices)
#
skulls <- read.csv("Egyptian skulls.csv",header=TRUE)
attach(skulls)
# Creating a data frame whose elements contain the means of each variable, by Period
means.df <- aggregate(skulls[,2:5], list(Period = Period), data=skulls, mean)
# The data.frame containing the means is ordered alphabetically by Period, thus,
# we reordered the periods according to the data 
means.listord <- means.df[order(c(3,1,2,4,5)),]  
skulls.mnv <- manova(as.matrix(skulls[,2:5])~ Period, data=skulls)
skulls.mnv
# summary.manova, a list, carries the Sum of Squares and Cross Product Matrices 
# in object SS. The list element summary.manova(skulls.mnv)$SS$Period is the
# Between-Samples (Period) Sum of Squares and Cross Products Matrix
BSSCPM <- summary.manova(skulls.mnv)$SS$Period
BSSCPM
# The list element summary.manova(skulls.mnv)$SS$Residuals 
# is the Within-Samples Sum of Squares and Cross Products Matrix
WSSCPM <- summary.manova(skulls.mnv)$SS$Residuals
WSSCPM
# The Total sum of squares and cross product matrix is the sum of B and W
TSSCPM <- BSSCPM + WSSCPM
TSSCPM
# The eigenanalysis of (W^(-1))*B will produce the coefficients of the canonical
# discriminant functions
discrim <- eigen(solve(WSSCPM)%*%BSSCPM)
discrim
# The eigenvectors are normalized to unit length, columnwise. 
(mateigen <- discrim$vectors)
X2 <- function(x) {x^2}
colSums(apply(mateigen,2,X2))
# Therefore, the coefficients of the canonical discriminant functions in
# Equation 8.2 do not match with the columns of mateigen
#
# Computing scores for individual skulls
scores.discrim <- as.matrix(skulls[,2:5])%*%mateigen
colnames(scores.discrim) <- c("CV1","CV2","CV3","CV4")
dat.scores <- cbind(data.frame(Period),scores.discrim)
# Getting means and standard deviations for the 4 discriminant functions
# with a different scale to that used in the set of the equations 8.2 
tabmeans.scores <- aggregate(dat.scores[,2:5], by=list(Period = Period), mean)
tabmeans.scores[c(2,3,1,4,5),]
tabsd.scores=aggregate(dat.scores[,2:5],by=list(Period=skulls$Period),sd)
tabsd.scores[c(2,3,1,4,5),]
# Remark: Scaling of eigenvectors in linear discriminant analysis in many packages assures
#         that e'Spool e = 1, where e is an eigenvector
# 
# Plot of Egyptian skulls against their values for the first two canonical
# discriminant functions.
plot(scores.discrim[,1],scores.discrim[,2], col=Period, pch=16, xlab="CV1", ylab="CV2")
legend("topleft", legend=unique(Period),col=unique(Period), pch=16, cex=0.9)
#
detach(skulls)
#
# End of script