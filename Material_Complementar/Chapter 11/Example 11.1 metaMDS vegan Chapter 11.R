# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 11. Multidimensional scaling
#
# Example 11.1: Road distances between New Zealand towns
# Non metric multidimensional scaling with metaMDS from "vegan" package
NZroads <- read.csv("Road distances NZ.csv", header=TRUE, row.names=1)
attach(NZroads)
# Before running library(vegan) make sure you have installed the package
library(vegan)
# Converts dataframe into a square matrix                  
MNZ <- as.matrix(NZroads)
# It is preferable to convert the square matrix into a dist class matrix (lower diagonal)      
dist.MNZ <- as.dist(MNZ)
dist.MNZ                        # Table 11.3
# Names of NZ cities       
cities <- row.names(MNZ)
# Function metaMDS performs Nonmetric Multidimensional Scaling (NMDS), 
# and tries to find a stable solution using several random starts.
# In addition, it standardizes the scaling in the result, so that the 
# configurations are easier to interpret. By default, a configuration with k=2 
# dimensions is searched. trymax = Maximum number of random starts in search of 
# stable solution (default = 20). Default distance is distance="bray" 
# (Bray-Curtis); here we use distance="euclidean". The option "autotransform" 
# uses simple heuristics for possible data transformation of typical community 
# data (rows are samples, columns are species). If you do not have community data,
# you should probably set autotransform = FALSE.
(MDSdim2NZ <- metaMDS(MNZ, distance="euclidean", autotransform=FALSE))
stress.dim2 <- MDSdim2NZ$stress
scores.sites <- scores(MDSdim2NZ,display="sites")
# Coordinates produced by multidimensional scaling in R are not the same as
# those shown in Table 11.4, but their configuration in the 2-dimensional space
# are similar after signs in the object scores.sites are reversed. This is seen
# in the map produced below 
plot(-scores.sites,type="n")
points(-scores.sites,pch=19,col="blue")
text(-scores.sites,lab=rownames(scores.sites),cex=0.7,pos=3)
text(200,-175,lab=paste("Stress = ",round(stress.dim2,3)))
title("Map of NZ South Island cities \n Nonmetric Multidimensional scaling using metaMDS")
#
detach(NZroads) 
#
# End of script
#