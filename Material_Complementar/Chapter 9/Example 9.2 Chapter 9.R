# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 9. Cluster analysis
#
# Example 9.2: Relationships between canine species
#
# Hirerarchic cluster analysis, Nearest-neighbor
canines <- read.csv("Canines means.csv", header=TRUE, row.names=1)
attach(canines)
# Standardizing the data with scale() 
matstd.can <- scale(canines)
# Creating a (Euclidean) distance matrix of the standardized data 
dist.canine <- dist(matstd.can, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)      
cluscanine.nn <- hclust(dist.canine, method = "single") 
# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom (Canine species' labels)
par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(cluscanine.nn),ylab="Distance between Canine species",ylim=c(0,2.5),
        main="Dendrogram of six canine species")
#
detach(canines)
#
# End of script
