# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 11. Multidimensional scaling
#
# Example 11.1: Road distances between New Zealand towns
#
# Non metric multidimensional scaling with isoMDS from "MASS" package
NZroads <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
attach(NZroads)
# Before running library(MASS) make sure you have installed the package
library(MASS)
# Converts dataframe into a square matrix                  
MNZ <- as.matrix(NZroads)
# It is preferable to convert the square matrix into a dist class matrix (lower diagonal)      
dist.MNZ <- as.dist(MNZ)
dist.MNZ                  # Table 11.3
# Names of NZ cities       
cities <- row.names(MNZ)       
# Kruskal's Non metric Multidimensional scaling
# isoMDS uses the Euclidean distance as the initial configuration, by default
# Running isoMDS with the defaults: k=2, tol= 1e-3, maxit=50
# The final stress achieved is displayed in percent.
MDSdim2NZiso1 <- isoMDS(dist.MNZ)    # Stress reached in this case is 7.3% (i.e 0.073)
MDSdim2NZiso1$stress
# This stress does not match the value given on page 207. Thus, we decide to
# run isoMDS with a larger number of iterations and smaller tolerance (but still k=2)
MDSdim2NZiso <- isoMDS(dist.MNZ,tol=1e-6,maxit=200)  
MDSdim2NZiso$stress           # Stress is 4% or 0.04
MDSdim2NZiso$points           # NMDS Coordinates 
# Coordinates produced by multidimensional scaling in R are not the same as
# those shown in Table 11.4, but their configuration in the 2-dimensional space
# are similar after signs in MDSdim2NZiso$points are reversed. This is seen
# in the map produced below 
# 
plot(-MDSdim2NZiso$points,type="p",xlab="Dimension 1",ylab="Dimension 2",
     main="Map of NZ South Island cities \n Nonmetric Multidimensional scaling using isoMDS",
     col="blue",pch=19)
text(-MDSdim2NZiso$points,labels=cities,cex=0.7,pos=3)
text(200,-175,paste("Stress = ",round(MDSdim2NZiso$stress/100,3)))
# Producing a Shepard plot
MDS2NZiso.Shep <- Shepard(dist.MNZ,MDSdim2NZiso$points,p=2)
dev.new()
plot(MDS2NZiso.Shep$yf, MDS2NZiso.Shep$x, pch = 15,cex=0.7,
     xlab="Configuration Distances",ylab="Data Dissimilarities",main="Shepard plot")
lines(MDS2NZiso.Shep$yf, MDS2NZiso.Shep$x, type = "S",col="red")
#
detach(NZroads)
#
# End of script
