# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 5. Measuring and testing multivariate distances
#
# Example 5.1: Distances between dogs and related species
#
canines <- read.csv("Prehistoric dogs.csv",header=TRUE,row.names=1)
attach(canines)
# Standardization of the canine data
matstdcan <- scale(canines)      # scale function standaridizes data
stdcanprt <- round(matstdcan, digits=2)
# Writing the standardized matrix in a csv file (Table 5.1, p. 86)
write.csv(stdcanprt,"Standardized canine data1.csv")
#
# Alternatively, decostand is a function in vegan that standardizes data too
# Before running library(vegan) make sure you have installed the package
# Remove the # in the next four lines get the standardized data with decostand 
# library(vegan)
# matstdcan1 <- decostand(canines,method="standardize")
# stdcanprt1 <- round(matstdcan, digits=2)
# write.csv(stdcanprt1,"Standardized canine data2.csv")  
#
# Computation of Euclidean distances between the seven canine groups (Table 5.2)
# Using function dist from package "stats"
dist1can <- dist(matstdcan,method = "euclidean")
# class of dist1can is "dist". It needs to be converted into "matrix" class
Stddistmat1 <- print(as.matrix(dist1can), digits=2) 
write.csv(Stddistmat1,"Std distance matrix canines1.csv")
#
# If you prefer to run vegdist from vegan package, remove the # in next 5 lines
# library(vegan)
# dist2can <- vegdist(matstdcan,method="euclidean")
# print(round(dist2can, digits=2))
# Stddistmat2 <- print(as.matrix(dist2can), digits=2) 
# write.csv(Stddistmat2,"Std distance matrix canines2.csv") 
#
detach(canines)
# 
# End of the sript