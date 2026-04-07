# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 11. Multidimensional scaling
#
# Example 11.1: Road distances between New Zealand towns
# Non metric multidimensional scaling with mds from package "smacof"
#
NZroads <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
attach(NZroads)
# Before running library(smacof) make sure you have installed the package
library(smacof)
# Converts dataframe into a square matrix            
MNZ <- as.matrix(NZroads)      
# It is preferable to convert the square matrix into a dist class matrix (lower diagonal)
dist.MNZ <- as.dist(MNZ)       
# Names of NZ cities       
cities <- row.names(MNZ) 
# The default method in smacof is PCO (type="ratio"). For nonmetric NMDS use
# type="ordinal. By default, ndim=2
MDSdim2NZsmacof <- mds(dist.MNZ, type="ordinal")
MDSdim2NZsmacof
# Coordinates produced by multidimensional scaling in R are not the same as
# those shown in Table 11.4, but their configuration in the 2-dimensional space
# are similar after signs in MDSdim2NZsmacof$conf are reversed. This is seen
# in the map produced below 
#
MDSdim2NZsmacof$conf        # Configuration of points in two dimensions
plot(-MDSdim2NZsmacof$conf,type="p",xlab="Dimension 1",ylab="Dimension 2",
     main="Map of NZ South Island cities \n Nonmetric Multidimensional scaling using mds",
     col="blue",pch=19)
text(-MDSdim2NZsmacof$conf,labels=cities,cex=0.7,pos=3)
text(0.5,-0.5,paste("Stress = ",round(MDSdim2NZsmacof$stress,3)))
#
detach(NZroads)
#
# End of script
#
