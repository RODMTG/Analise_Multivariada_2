# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 11. Multidimensional scaling
#
# Example 11.2: The voting behavior of congressmen
#
Congmen <- read.csv("Voting NJ Congressmen.csv",header=TRUE,row.names=1)
attach(Congmen)
# Converts dataframe into a square matrix
Vote.mat <- as.matrix(Congmen) 
colnames(Vote.mat) <- rownames(Vote.mat)
# It is preferable to convert the square matrix into a dist class matrix (lower diagonal)
dist.Vote <- as.dist(Vote.mat)
dist.Vote                       # Table 11.5
# Running classic metric multidimensional scaling first (Principal Coordinate Analysis),
# as described on page 210. The stress function is created here (stress.fun) 
# in order to compute the stress for each dimension (k=2,3,4).
stress.fun <- function(datadist,fitteddist) {
                       sqrt(sum((datadist-fitteddist)^2)/sum(datadist^2))
              }
MDSdim2Vote.cmd <- cmdscale(dist.Vote, eig=TRUE, k=2)
(stress.2d.cmd <- stress.fun(dist.Vote,dist(MDSdim2Vote.cmd$points)))
MDSdim3Vote.cmd <- cmdscale(dist.Vote, eig=TRUE, k=3)
(stress.3d.cmd <- stress.fun(dist.Vote,dist(MDSdim3Vote.cmd$points)))
MDSdim4Vote.cmd <- cmdscale(dist.Vote, eig=TRUE, k=4)
(stress.4d.cmd <- stress.fun(dist.Vote,dist(MDSdim4Vote.cmd$points)))
#
# We run non metric multidimensional scaling with isoMDS from package MASS
# Defaults for the number of iterations and tolerance are used
# Before running library(MASS) make sure you have installed the package
library(MASS)
MDSdim2Vote.iso <- isoMDS(dist.Vote)        # k=2
MDSdim3Vote.iso <- isoMDS(dist.Vote, k=3)
MDSdim4Vote.iso <- isoMDS(dist.Vote, k=4)
# Remark: the configuration produced by isoMDS is only determined up to rotations 
# and reflections. Thus, the result can vary considerably from machine to machine
# This explains why the stress in three dimensions and plots do not match 
# exactly the results given in Example 11.2
#
# Plots similar to those shown in Figure 11.5 are displayed as two separate plots
# Dimension 1 vs Dimension 2
plot(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,2], pch=16, col="blue",
     xlab="Dimension 1",ylab="Dimension 2", xlim=c(-10,8), ylim=c(-5,8),
     main="isoMDS function")
text(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,2],
       labels=rownames(MDSdim3Vote.iso$points), cex=0.7, pos=3)
abline(h=0)
abline(v=0)
# Dimension 1 vs Dimension 3
dev.new()
plot(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,3], pch=16, col="blue",
     xlab="Dimension 1",ylab="Dimension 3", xlim=c(-10,8), ylim=c(-5,8),
     main="isoMDS function")
text(MDSdim3Vote.iso$points[,1],MDSdim3Vote.iso$points[,3],
       labels=rownames(MDSdim3Vote.iso$points), cex=0.7, pos=3)
abline(h=0)
abline(v=0)
# Producing a Shepard plot
MDS2Voteiso.Shep <- Shepard(dist.Vote,MDSdim3Vote.iso$points, p=3)
dev.new()
plot(MDS2Voteiso.Shep$yf,MDS2Voteiso.Shep$x, pch = 15,cex=0.7,
     xlab="Configuration Distance",ylab="Data Dissimilarities",main="Shepard plot")
lines(MDS2Voteiso.Shep$yf,MDS2Voteiso.Shep$x, type = "S",col="red")
#
# Now we run non metric multidimensional scaling with metaMDS from package "vegan“
# This procedure uses 20 random starts for stress minimization. Results will not
# match those given in the book, algorithms in different programs may vary
# Before running library(vegan) make sure you have installed the package.
library(vegan)
#
# Function metaMDS performs Nonmetric Multidimensional Scaling (NMDS), 
# and tries to find a stable solution using several random starts.
# In addition, it standardizes the scaling in the result, so that the 
# configurations are easier to interpret. By default, a configuration with k=2 
# dimensions is searched. Here we also produce NMDS for k=3 and k=4.
# trymax = Maximum number of random starts in search of stable solution 
# (default = 20). Default distance is distance="bray" (Bray-Curtis); here we use
# distance="euclidean". The option "autotransform" uses simple heuristics for 
# possible data transformation of typical community data (rows are samples, 
# columns are species). If you do not have community data, you should probably 
# set autotransform = FALSE.
(MDSdim2Vote.meta <- metaMDS(dist.Vote, k=2, autotransform=FALSE))
(stress.dim2 <- MDSdim2Vote.meta$stress)
(MDSdim3Vote.meta <- metaMDS(dist.Vote, k=3, autotransform=FALSE))
(stress.dim3 <- MDSdim3Vote.meta$stress)
(MDSdim4Vote.meta <- metaMDS(dist.Vote, k=4, autotransform=FALSE))
(stress.dim4 <- MDSdim4Vote.meta$stress)
scores.sites3 <- scores(MDSdim3Vote.meta, display="sites")
# Displaying the 3-dimensional configuration by means of two 2-dimensional plots
# Dimension 1 vs Dimension 2
dev.new()
plot(scores.sites3[,1:2],type="n",xlim=c(-10,7), ylim=c(-6,9), main="metaMDS function")
points(scores.sites3[,1:2],pch=16,col="blue")
text(scores.sites3[,1:2],lab=rownames(scores.sites3),cex=0.7,pos=3)
abline(h=0)
abline(v=0)
# Dimension 1 vs Dimension 3
dev.new()
plot(scores.sites3[,c(1,3)],type="n",xlim=c(-10,7), ylim=c(-6,8), main="isoMDS function")
points(scores.sites3[,c(1,3)],pch=16,col="blue")
text(scores.sites3[,c(1,3)],lab=rownames(scores.sites3),cex=0.7,pos=3)
abline(h=0)
abline(v=0)
# 
# Finally we run nonmetric multidimensional scaling using mds from smacof package
# The default method in smacof is PCO (type="ratio"). For nonmetric NMDS use
# type="ordinal. Here we asked for 2- (default), 3- and 4- dimensional configurations
# Before running library(vegan) make sure you have installed the package.
library(smacof)
MDSdim2congsmacof <- mds(dist.Vote, type="ordinal")
MDSdim2congsmacof
MDSdim3congsmacof <- mds(dist.Vote, type="ordinal", ndim=3)
MDSdim3congsmacof
MDSdim4congsmacof <- mds(dist.Vote, type="ordinal", ndim=4)
MDSdim4congsmacof
# Scatterplots of the scores for the 3-dimensional configuration
# Dimension 1 vs Dimension 2
dev.new()
plot(MDSdim3congsmacof$conf[,c("D1","D2")],type="p",, pch=16, col="blue",
     xlab="Dimension 1",ylab="Dimension 2", xlim=c(-1.1,0.9), ylim=c(-0.45,0.75),
     main="mds function")
text(MDSdim3congsmacof$conf[,c("D1","D2")],labels=rownames(MDSdim3congsmacof$conf),cex=0.7,pos=3)
abline(h=0)
abline(v=0)
# Dimension 1 vs Dimension 3
dev.new()
plot(MDSdim3congsmacof$conf[,c("D1","D3")],type="p",, pch=16, col="blue",
     xlab="Dimension 1",ylab="Dimension 3", xlim=c(-1.1,0.9), ylim=c(-0.45,0.75),
     main="mds function")
text(MDSdim3congsmacof$conf[,c("D1","D3")],labels=rownames(MDSdim3congsmacof$conf),cex=0.7,pos=3)
abline(h=0)
abline(v=0)
#
detach(Congmen) 
# 
# End of script
